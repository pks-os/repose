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
import java.nio.charset.StandardCharsets
import java.util.Collections

import com.atlassian.oai.validator.SwaggerRequestResponseValidator
import com.atlassian.oai.validator.model.{Request, SimpleRequest}
import com.atlassian.oai.validator.report.ValidationReport
import com.atlassian.oai.validator.report.ValidationReport.Level
import com.typesafe.scalalogging.slf4j.StrictLogging
import javax.inject.Named
import javax.servlet._
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import org.openrepose.commons.utils.io.BufferedServletInputStream
import org.openrepose.commons.utils.servlet.http.HttpServletRequestWrapper
import org.openrepose.filters.openapivalidator.OpenApiValidatorFilter._

import scala.collection.JavaConverters._
import scala.io.Source

/**
  * This filter will validate requests against an OpenAPI definition.
  *
  * While the underlying validation library is a great start,
  * I would suggest the following enhancements:
  * - Report messages should be typed for easier analysis
  * - Custom validations support is necessary for vendor extension validations (e.g., RBAC).
  * - Abstract the core validation and reporting logic so that it is not tied to OpenAPI.
  * That is, do not use OpenAPI model objects in the core, and provide a model for requests/responses.
  * Doing so would enable bindings to exist multiple API definition formats (e.g., OpenAPI, RAML, WADL).
  */
@Named
class OpenApiValidatorFilter extends Filter with StrictLogging {

  // fixme: this currently always loads a swagger file resource included in the artifact,
  // fixme: but should load a file provided by configuration
  private val validator: SwaggerRequestResponseValidator = SwaggerRequestResponseValidator
    .createFor("swagger.yaml")
    .build()

  override def init(filterConfig: FilterConfig): Unit = {
    logger.trace("init called")
  }

  override def doFilter(request: ServletRequest, response: ServletResponse, chain: FilterChain): Unit = {
    logger.trace("doFilter called")

    val httpServletRequest = request.asInstanceOf[HttpServletRequest]
    val httpServletResponse = response.asInstanceOf[HttpServletResponse]
    val bufferedRequestStream = new BufferedServletInputStream(httpServletRequest.getInputStream)
    val wrappedHttpServletRequest = new HttpServletRequestWrapper(httpServletRequest, bufferedRequestStream)

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
    val errorStatus = validationReport.getMessages.asScala
      .collectFirst({ case Issue(issue) => issue })

    errorStatus match {
      case Some(issue) => httpServletResponse.sendError(issue.statusCode, issue.message)
      case None => chain.doFilter(wrappedHttpServletRequest, response)
    }
  }

  override def destroy(): Unit = {
    logger.trace("destroy called")
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
