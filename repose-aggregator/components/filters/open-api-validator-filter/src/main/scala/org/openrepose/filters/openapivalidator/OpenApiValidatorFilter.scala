/*
 * _=_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_=
 * Repose
 * _-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
 * Copyright (C) 2010 - 2015 Rackspace US, Inc.
 * _-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * =_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_=_
 */
package org.openrepose.filters.openapivalidator

import java.io.InputStream
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import java.util.Collections

import com.atlassian.oai.validator.SwaggerRequestResponseValidator
import com.atlassian.oai.validator.model.{Request, SimpleRequest}
import com.atlassian.oai.validator.report.ValidationReport
import com.atlassian.oai.validator.report.ValidationReport.Level
import javax.inject.{Inject, Named}
import javax.servlet._
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import org.openrepose.commons.utils.io.BufferedServletInputStream
import org.openrepose.commons.utils.servlet.http.HttpServletRequestWrapper
import org.openrepose.core.filter.AbstractConfiguredFilter
import org.openrepose.core.services.config.ConfigurationService
import org.openrepose.core.spring.ReposeSpringProperties
import org.openrepose.filters.openapivalidator.OpenApiValidatorFilter._
import org.openrepose.filters.openapivalidator.config.OpenApiValidatorConfig
import org.springframework.beans.factory.annotation.Value

import scala.collection.JavaConverters._
import scala.io.Source

/**
  * This filter will validate requests against an OpenAPI definition.
  *
  * While the underlying validation library is a great start, the following would need to be addressed:
  * - Report messages should be typed for easier analysis.
  *   - We can kind work around this by mapping message keys to types.
  * - Vendor extensions support is necessary (exposing vendor extensions at various level during validation).
  *   - Will require exposing the Path model in addition to the Operation model
  * - Custom validations support is necessary for vendor extension validations (e.g., RBAC).
  *
  * My wishlist for the underlying validation library would include:
  * - Abstract the core validation and reporting logic so that it is not tied to OpenAPI. That is,
  *   do not use OpenAPI model objects in the core, and provide a model for requests/responses.
  *   Doing so would enable bindings to exist multiple API definition formats (e.g., OpenAPI, RAML, WADL).
  * - Metrics. With proper metrics, we could provide API coverage functionality like api-checker.
  */
@Named
class OpenApiValidatorFilter @Inject()(@Value(ReposeSpringProperties.CORE.CONFIG_ROOT) configurationRoot: String,
                                       configurationService: ConfigurationService)
  extends AbstractConfiguredFilter[OpenApiValidatorConfig](configurationService) {

  override val DEFAULT_CONFIG: String = "open-api-validator.cfg.xml"
  override val SCHEMA_LOCATION: String = "/META-INF/schema/config/open-api-validator.xsd"

  private var validator: SwaggerRequestResponseValidator = _

  override def doWork(httpRequest: HttpServletRequest, httpResponse: HttpServletResponse, chain: FilterChain): Unit = {
    val bufferedRequestStream = new BufferedServletInputStream(httpRequest.getInputStream)
    val wrappedHttpServletRequest = new HttpServletRequestWrapper(httpRequest, bufferedRequestStream)

    bufferedRequestStream.mark(Integer.MAX_VALUE)

    // todo: add extension checks (e.g., role check) to validation
    // todo: this will use vendor extensions from the {{Operation}} Swagger model
    // todo: since the validator library does not have hooks into the underlying OpenAPI model, we may have to parse
    // todo: the OpenAPI document again with a parser, but I would rather add extended validation (i.e., user-provided)
    // todo: to the library
    val validationReport = validator.validateRequest(HttpServletOAIRequest(wrappedHttpServletRequest))

    bufferedRequestStream.reset()

    // todo: prioritize failures in a flexible way so that the correct failure can be selected and the correct
    // todo: response can be returned
    // todo: annoyingly, path and method checks are handled separately from all others (forcing the validation
    // todo:   ordering to be path -> method -> all other checks) and return early on failure
    val errorStatus = validationReport.getMessages.asScala
      .collectFirst({ case Issue(issue) => issue })

    errorStatus match {
      case Some(issue) => httpResponse.sendError(issue.statusCode, issue.message)
      case None => chain.doFilter(wrappedHttpServletRequest, httpResponse)
    }
  }

  override def doConfigurationUpdated(newConfiguration: OpenApiValidatorConfig): OpenApiValidatorConfig = {
    validator = SwaggerRequestResponseValidator
      .createFor(resolveHref(newConfiguration.getHref))
      .build()

    newConfiguration
  }

  /**
    * Returns a [[String]] representation of an absolute [[URI]].
    *
    * This method will resolve relative [[URI]] representations as files
    * relative to the configuration root directory.
    *
    * @param href a [[String]] representation of a [[URI]]
    * @return a [[String]] representation of an absolute [[URI]]
    */
  private def resolveHref(href: String): String = {
    val hrefUri = URI.create(href)
    if (hrefUri.isAbsolute) {
      // The URI is absolute, so return it as-is.
      // This handles hrefs pointing to remote resources (e.g., HTTP, FTP).
      hrefUri.toString
    } else {
      // The URI is relative, so process it as a file.
      val oaiDocumentPath = Paths.get(href)
      if (oaiDocumentPath.isAbsolute) {
        // The file path is absolute, so return the absolute URI for the file path.
        oaiDocumentPath.toUri.toString
      } else {
        // The file path is relative, so resolve it relative to the configuration directory
        // and return the absolute URI.
        Paths.get(configurationRoot).resolve(oaiDocumentPath).toUri.toString
      }
    }
  }
}

object OpenApiValidatorFilter {

  sealed trait Issue {
    val statusCode: Int
    val message: String
  }

  case class UnknownIssue(message: String) extends Issue {
    override val statusCode: Int = HttpServletResponse.SC_BAD_REQUEST
  }

  case class PathIssue(message: String) extends Issue {
    override val statusCode: Int = HttpServletResponse.SC_NOT_FOUND
  }

  case class MethodIssue(message: String) extends Issue {
    override val statusCode: Int = HttpServletResponse.SC_METHOD_NOT_ALLOWED
  }

  object Issue {
    def unapply(message: ValidationReport.Message): Option[Issue] = {
      if (message.getLevel != Level.ERROR) {
        None
      } else {
        Some(
          message.getKey match {
            case "validation.request.path.missing" => PathIssue(message.getMessage)
            case "validation.request.operation.notAllowed" => MethodIssue(message.getMessage)
            case _ => UnknownIssue(message.getMessage)
          }
        )
      }
    }
  }

  object HttpServletOAIRequest {
    def apply(httpServletRequest: HttpServletRequest): Request = {
      // fixme: handle unsupported encodings gracefully
      val encoding = Option(httpServletRequest.getCharacterEncoding).getOrElse(StandardCharsets.ISO_8859_1.name)
      val builder = new SimpleRequest.Builder(httpServletRequest.getMethod, httpServletRequest.getRequestURI)
        .withBody(inputStreamToString(httpServletRequest.getInputStream, encoding))

      httpServletRequest.getHeaderNames.asScala.foreach { headerName =>
        builder.withHeader(headerName, Collections.list(httpServletRequest.getHeaders(headerName)))
      }

      // fixme: convert query parameters

      builder.build()
    }
  }

  private def inputStreamToString(inputStream: InputStream, encoding: String): String = {
    Source.fromInputStream(inputStream, encoding)
      .getLines
      .mkString
  }
}
