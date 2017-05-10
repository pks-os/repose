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
package org.openrepose.filters.attributemapping

import java.io.ByteArrayInputStream
import javax.servlet.http.HttpServletResponse
import javax.servlet.http.HttpServletResponse.SC_UNSUPPORTED_MEDIA_TYPE
import javax.xml.transform.Source

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterEach, FunSpec, Matchers}
import org.springframework.http.MediaType
import org.springframework.mock.web.{MockFilterChain, MockHttpServletRequest, MockHttpServletResponse}

import scala.util.{Failure, Success}

@RunWith(classOf[JUnitRunner])
class AttributeMappingPolicyValidationFilterTest
  extends FunSpec with Matchers with BeforeAndAfterEach {

  import AttributeMappingPolicyValidationFilterTest._

  var request: MockHttpServletRequest = _
  var response: MockHttpServletResponse = _
  var chain: MockFilterChain = _

  var filter: AttributeMappingPolicyValidationFilter = _

  override def beforeEach(): Unit = {
    request = new MockHttpServletRequest()
    response = new MockHttpServletResponse()
    chain = new MockFilterChain()

    filter = new AttributeMappingPolicyValidationFilter()
  }

  describe("validateHttpMethod") {
    it("should return a Success if the method is PUT") {
      request.setMethod("PUT")

      filter.validateHttpMethod(request) shouldBe a[Success[_]]
    }

    HttpMethods foreach { method =>
      it(s"should return a Failure if the method is $method") {
        request.setMethod(method)

        filter.validateHttpMethod(request) shouldBe a[Failure[_]]
      }
    }
  }

  describe("getPolicyAsXmlSource") {
    SupportedMediaTypes foreach { mediaType =>
      it(s"should return a Source if the content-type is $mediaType") {
        filter.getPolicyAsXmlSource(mediaType, new ByteArrayInputStream(ValidJsonPolicy.getBytes)) shouldBe a[Success[Source]]
      }
    }

    it("should return a Failure if the content-type is not supported") {
      filter.getPolicyAsXmlSource("text/plain", new ByteArrayInputStream(ValidJsonPolicy.getBytes)) shouldBe a[Failure[_]]
    }
  }

  describe("doFilter") {
    it("should return a 415 if the Content-Type is not supported") {
      request.setMethod("PUT")
      request.setContentType("text/plain")

      filter.doFilter(request, response, chain)

      response.isCommitted shouldBe true
      response.getStatus shouldBe SC_UNSUPPORTED_MEDIA_TYPE
    }

    it("should forward a normalized policy if validation succeeds") {
      request.setMethod("PUT")
      request.setContent(ValidJsonPolicy.getBytes)
      request.setContentType(MediaType.APPLICATION_JSON_VALUE)

      filter.doFilter(request, response, chain)

      chain.getRequest should not be theSameInstanceAs(request)
      chain.getResponse shouldBe theSameInstanceAs(response)
    }

    it("should return a 400 if the policy fails to validate") {
      request.setMethod("PUT")
      request.setContent(InvalidJsonPolicy.getBytes)
      request.setContentType(MediaType.APPLICATION_JSON_VALUE)

      filter.doFilter(request, response, chain)

      response.getStatus shouldBe HttpServletResponse.SC_BAD_REQUEST
      response.isCommitted shouldBe true
    }

    HttpMethods foreach { method =>
      it(s"should pass $method requests through the filter") {
        request.setMethod(method)

        filter.doFilter(request, response, chain)

        chain.getRequest shouldBe theSameInstanceAs(request)
        chain.getResponse shouldBe theSameInstanceAs(response)
      }
    }
  }
}

object AttributeMappingPolicyValidationFilterTest {
  final val HttpMethods = Set(
    "GET",
    "DELETE",
    "POST",
    "PATCH",
    "HEAD",
    "OPTIONS",
    "CONNECT",
    "TRACE")

  final val SupportedMediaTypes = Set(
    MediaType.APPLICATION_JSON_VALUE,
    MediaType.APPLICATION_XML_VALUE)

  final val ValidJsonPolicy: String =
    """
      |{
      |  "mapping": {
      |    "rules": [
      |       {
      |        "local": {
      |          "user": {
      |            "domain":"{D}",
      |            "name":"{D}",
      |            "email":"{D}",
      |            "roles":"{D}",
      |            "expire":"{D}"
      |          }
      |        }
      |      }
      |    ],
      |    "version":"RAX-1"
      |  }
      |}
    """.stripMargin

  final val InvalidJsonPolicy: String =
    """
      |{
      |  "mapping": {
      |    "rules": {
      |       {
      |        "local": {
      |          "user": {
      |            "domain":"{D}",
      |            "name":"{D}",
      |            "email":"{D}",
      |            "roles":"{D}",
      |            "expire":"{D}"
      |          }
      |        }
      |      }
      |    },
      |    "version":"RAX-1"
      |  }
      |}
    """.stripMargin
}
