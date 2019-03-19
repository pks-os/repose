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

import java.net.URI
import java.nio.file.Paths

import com.atlassian.oai.validator.OpenApiInteractionValidator
import com.atlassian.oai.validator.report.ValidationReport
import com.typesafe.scalalogging.slf4j.StrictLogging
import javax.inject.{Inject, Named}
import javax.servlet._
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import org.openrepose.commons.utils.io.BufferedServletInputStream
import org.openrepose.commons.utils.servlet.http.HttpServletRequestWrapper
import org.openrepose.core.filter.AbstractConfiguredFilter
import org.openrepose.core.services.config.ConfigurationService
import org.openrepose.core.spring.ReposeSpringProperties
import org.openrepose.filters.openapivalidator.config.OpenApiValidatorConfig
import org.springframework.beans.factory.annotation.Value

import scala.collection.JavaConverters._

/**
  * This filter will validate requests against an OpenAPI document.
  */
@Named
class OpenApiValidatorFilter @Inject()(@Value(ReposeSpringProperties.CORE.CONFIG_ROOT) configurationRoot: String,
                                       configurationService: ConfigurationService)
  extends AbstractConfiguredFilter[OpenApiValidatorConfig](configurationService) with StrictLogging {

  import org.openrepose.filters.openapivalidator.OpenApiValidatorFilter._

  override final val DEFAULT_CONFIG: String = "open-api-validator.cfg.xml"
  override final val SCHEMA_LOCATION: String = "/META-INF/schema/config/open-api-validator.xsd"

  private var validator: OpenApiInteractionValidator = _

  override def doWork(httpRequest: HttpServletRequest, httpResponse: HttpServletResponse, chain: FilterChain): Unit = {
    // Ensure that request input stream buffering is supported and wrap the request so that components further
    // down the chain can call getInputStream or getReader.
    val bufferingRequestInputStream = getBufferingServletInputStream(httpRequest.getInputStream)
    val wrappedHttpServletRequest = new HttpServletRequestWrapper(httpRequest, bufferingRequestInputStream)

    bufferingRequestInputStream.mark(Integer.MAX_VALUE)

    logger.trace("Converting servlet request to validation library request")
    val validationRequest = HttpServletOAIRequest(wrappedHttpServletRequest)

    // TODO: Handle RequestConversionException
    logger.debug("Validating request")
    val validationReport = validator.validateRequest(validationRequest)

    bufferingRequestInputStream.reset()

    // Maps all error Messages to Issues, sorts the Issues by priority, and returns the highest priority Issue,
    // if one is present.
    // Note that a report may contain Messages for more than one failed check.
    // Note that we have chosen to ignore unknown issues here (i.e., remove issues that are not present in our issue map).
    // In doing so, we are effectively only supporting issues that we have mapped out ourselves.
    // As a result, the behavior of this filter should be knowable without requiring intimate knowledge
    // of the underlying validation library.
    // Additionally, the underlying validation library can be upgraded while the behavior of this filter
    // remains relatively stable (since any new issues would not necessarily need to be accounted for
    // at the time of the upgrade).
    // Furthermore, this approach provides a way to add functionality iteratively.
    // For example, if this filter fails to map an attribute of the servlet request to a request
    // for validation, the request is passed rather than rejected.
    // In that case, rejection would be inappropriate since the request may satisfy the required
    // criterion, our mapping simply does not provide the information to make an accurate determination.
    val priorityValidationFailure = validationReport.getMessages.asScala
      .filter(message => message.getLevel == ValidationReport.Level.ERROR)
      .flatMap(message => ValidationIssues.get(message.getKey).map(issue => ValidationFailure(issue, message)))
      .sortBy(issue => issue.issue.priority)
      .reverse
      .headOption

    priorityValidationFailure match {
      case Some(validationFailure) =>
        logger.info("Failed to validate request -- rejecting with status code: {} for reason: {}", validationFailure.issue.statusCode.toString, validationFailure.message)
        httpResponse.sendError(validationFailure.issue.statusCode, validationFailure.message.getMessage)
      case None =>
        logger.trace("Successfully validated request -- continuing processing")
        chain.doFilter(wrappedHttpServletRequest, httpResponse)
    }
  }

  override def doConfigurationUpdated(newConfiguration: OpenApiValidatorConfig): OpenApiValidatorConfig = {
    validator = OpenApiInteractionValidator
      .createFor(resolveHref(newConfiguration.getHref))
      .build()

    newConfiguration
  }

  /**
    * Resolves relative [[URI]] representations as files relative to the configuration root directory.
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

object OpenApiValidatorFilter extends StrictLogging {

  // A mapping from Message keys defined by the validation library to data defining how we will handle those Messages.
  // Note that the path and method checks necessarily occur before all other checks since they are required
  // to resolve the API Operation in the OpenAPI document.
  // If either one fails, no further checks will be performed.
  // As such, the validation order will be (path -> method -> everything else).
  // For that reason, path and method issues are given the highest and second highest priorities respectively.
  // TODO: Handle the remaining messages (that we care about)
  final val ValidationIssues: Map[String, ValidationIssue] = Map(
    "validation.request.path.missing" -> ValidationIssue(HttpServletResponse.SC_NOT_FOUND, Int.MaxValue),
    "validation.request.operation.notAllowed" -> ValidationIssue(HttpServletResponse.SC_METHOD_NOT_ALLOWED, Int.MaxValue - 1),
    "validation.request.body.unexpected" -> ValidationIssue(HttpServletResponse.SC_BAD_REQUEST, 1),
    "validation.request.body.missing" -> ValidationIssue(HttpServletResponse.SC_BAD_REQUEST, 2),
    "validation.request.parameter.header.missing" -> ValidationIssue(HttpServletResponse.SC_BAD_REQUEST, 3)
    //  "validation.request.contentType.invalid"
    //  "validation.request.contentType.notAllowed"
    //  "validation.request.accept.invalid"
    //  "validation.request.accept.notAllowed"
  )

  case class ValidationIssue(statusCode: Int, priority: Int)

  case class ValidationFailure(issue: ValidationIssue, message: ValidationReport.Message)

  /**
    * @param servletInputStream a [[ServletInputStream]]
    * @return a [[ServletInputStream]] that supports [[ServletInputStream#mark]] and [[ServletInputStream#reset]]
    */
  private def getBufferingServletInputStream(servletInputStream: ServletInputStream): ServletInputStream = {
    if (servletInputStream.markSupported) {
      servletInputStream
    } else {
      new BufferedServletInputStream(servletInputStream)
    }
  }
}
