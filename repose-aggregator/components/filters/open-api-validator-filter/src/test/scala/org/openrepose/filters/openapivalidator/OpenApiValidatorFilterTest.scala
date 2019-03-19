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

import com.atlassian.oai.validator.OpenApiInteractionValidator.ApiLoadException
import com.atlassian.oai.validator.model.Request
import org.junit.runner.RunWith
import org.openrepose.core.services.config.ConfigurationService
import org.openrepose.filters.openapivalidator.OpenApiValidatorFilter.HttpServletOAIRequest
import org.openrepose.filters.openapivalidator.config.OpenApiValidatorConfig
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.MockitoSugar
import org.scalatest.{BeforeAndAfterEach, Entry, FunSpec, Matchers}
import org.springframework.http.{HttpHeaders, MediaType}
import org.springframework.mock.web.MockHttpServletRequest

import scala.io.Source

@RunWith(classOf[JUnitRunner])
class OpenApiValidatorFilterTest
  extends FunSpec with BeforeAndAfterEach with MockitoSugar with Matchers {

  import OpenApiValidatorFilterTest._

  final val ConfigRoot: String = "unused"

  var configurationService: ConfigurationService = _
  var openApiValidatorFilter: OpenApiValidatorFilter = _

  override protected def beforeEach(): Unit = {
    super.beforeEach()

    configurationService = mock[ConfigurationService]
    openApiValidatorFilter = new OpenApiValidatorFilter(ConfigRoot, configurationService)
  }

  describe("doWork") {
    it("should pass on a request with no validation issues") {
      pending
    }

    it("should not destructively read the request body") {
      pending
    }

    it("should wrap the request input stream if mark is not supported") {
      pending
    }

    it("should not wrap the request input stream if mark is supported") {
      pending
    }

    it("should report the highest priority validation issue") {
      pending
    }
  }

  describe("doConfigurationUpdated") {
    it("should fail to load a non-existent OpenAPI document") {
      val href = "file:/invalid/path.yaml"
      val config = new OpenApiValidatorConfig
      config.setHref(href)

      an[ApiLoadException] should be thrownBy openApiValidatorFilter.doConfigurationUpdated(config)
    }

    it("should fail to load an invalid OpenAPI document") {
      val href = getClass.getResource("/openapi/invalid.yaml").toString
      val config = new OpenApiValidatorConfig
      config.setHref(href)

      an[ApiLoadException] should be thrownBy openApiValidatorFilter.doConfigurationUpdated(config)
    }

    Set("v2/petstore.json", "v2/petstore.yaml", "v3/petstore.yaml").foreach { document =>
      it(s"should successfully parse the $document OpenAPI document") {
        val href = getClass.getResource(s"/openapi/$document").toString
        val config = new OpenApiValidatorConfig
        config.setHref(href)

        noException should be thrownBy openApiValidatorFilter.doConfigurationUpdated(config)
      }
    }
  }

  describe("HttpServletOAIRequest") {
    describe("apply") {
      Request.Method.values.map(_.name).foreach { method =>
        it(s"should translate a servlet $method request to a validator request") {
          val headerEntries = Seq(
            Entry("Test-Header", "one,two"),
            Entry("Test-Header", "three"),
            Entry("Other-Test-Header", "four")
          )
          val servletRequest = new MockHttpServletRequest(method, "/test/path")
          headerEntries.foreach(headerEntry => servletRequest.addHeader(headerEntry.key, headerEntry.value))

          val validatorRequest = HttpServletOAIRequest.apply(servletRequest)

          validatorRequest.getPath shouldBe servletRequest.getRequestURI
          validatorRequest.getMethod.name shouldBe servletRequest.getMethod
          validatorRequest.getHeaders.keySet should contain only("Test-Header", "Other-Test-Header")
          validatorRequest.getHeaderValues("Test-Header") should contain only("one", "two", "three")
          validatorRequest.getHeaderValues("Other-Test-Header") should contain only "four"
        }
      }

      it("should fail to translate a servlet request with an unsupported method") {
        val servletRequest = new MockHttpServletRequest("FOO", "/")

        an[IllegalArgumentException] should be thrownBy HttpServletOAIRequest.apply(servletRequest)
      }

      Set(
        StandardCharsets.ISO_8859_1,
        StandardCharsets.US_ASCII,
        StandardCharsets.UTF_8,
        StandardCharsets.UTF_16,
        StandardCharsets.UTF_16BE,
        StandardCharsets.UTF_16LE
      ).foreach { charset =>
        it(s"should translate a $charset encoded servlet request body to a validator request") {
          val servletRequest = new MockHttpServletRequest(Request.Method.POST.name, "/test/path")
          servletRequest.setContent("Lorem ipsum".getBytes(charset))
          servletRequest.setContentType(MediaType.TEXT_PLAIN_VALUE)
          servletRequest.setCharacterEncoding(charset.name)
          servletRequest.addHeader(HttpHeaders.CONTENT_LENGTH, servletRequest.getContentLength)

          val validatorRequest = HttpServletOAIRequest.apply(servletRequest)

          validatorRequest.getBody.get shouldBe inputStreamToString(servletRequest.getInputStream, servletRequest.getCharacterEncoding)
          validatorRequest.getContentType.get shouldBe servletRequest.getHeader(HttpHeaders.CONTENT_TYPE)
          validatorRequest.getHeaderValue(HttpHeaders.CONTENT_LENGTH).get.toInt shouldBe servletRequest.getContentLength
        }
      }

      it("should ignore a servlet request body in an unsupported character encoding") {
        val requestCharacterEncoding = "FOOBAR"
        val servletRequest = new MockHttpServletRequest(Request.Method.POST.name, "/test/path")
        servletRequest.setContent("Lorem ipsum".getBytes)
        servletRequest.setContentType(MediaType.TEXT_PLAIN_VALUE)
        servletRequest.setCharacterEncoding(requestCharacterEncoding)
        servletRequest.addHeader(HttpHeaders.CONTENT_LENGTH, servletRequest.getContentLength)

        val validatorRequest = HttpServletOAIRequest.apply(servletRequest)

        validatorRequest.getBody.isPresent shouldBe false
        validatorRequest.getContentType.get shouldBe servletRequest.getHeader(HttpHeaders.CONTENT_TYPE)
        validatorRequest.getHeaderValue(HttpHeaders.CONTENT_LENGTH).get.toInt shouldBe servletRequest.getContentLength
      }
    }
  }
}

object OpenApiValidatorFilterTest {
  def inputStreamToString(inputStream: InputStream, encoding: String): String = {
    Source.fromInputStream(inputStream, encoding)
      .getLines
      .mkString(System.lineSeparator)
  }
}
