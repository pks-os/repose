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
package features.core.valveselfconfigure

import org.junit.experimental.categories.Category
import org.openrepose.framework.test.ReposeValveTest
import org.rackspace.deproxy.Deproxy
import scaffold.category.Core
import spock.lang.Shared

import java.util.concurrent.TimeUnit

@Category(Core)
class StartWithZeroNodesTest extends ReposeValveTest {

    @Shared
    int port

    def setupSpec() {

        deproxy = new Deproxy()
        deproxy.addEndpoint(properties.targetPort)

        port = properties.reposePort

        def params = properties.getDefaultTemplateParams()
        params += [
                'host': 'example.com',
                'port': port,
        ]

        repose.configurationProvider.cleanConfigDirectory()
        repose.configurationProvider.applyConfigs("common", params)
        repose.configurationProvider.applyConfigs("features/core/valveselfconfigure/container-no-port", params)
        repose.configurationProvider.applyConfigs("features/core/valveselfconfigure/zero-nodes", params)
        repose.start(killOthersBeforeStarting: false, waitOnJmxAfterStarting: false)
        reposeLogSearch.awaitByString(
            "Configuration update error. Reason: Parsed object from XML does not match the expected " +
                "configuration class. Expected: org.openrepose.core.systemmodel.config.SystemModel",
            1,
            35,
            TimeUnit.SECONDS
        )
    }

    def "when we start with zero nodes in the system model, then switch to a system model with one  localhost node"() {

        when: "Repose first starts up with zero nodes"
        deproxy.makeRequest(url: "http://localhost:${port}")
        then: "it should not connect"
        thrown(ConnectException)

        when: "change the configs while it's running - add a single localhost node"
        def params = properties.getDefaultTemplateParams()
        params += [
                'host': 'localhost',
                'port': port,
        ]
        repose.configurationProvider.applyConfigs('features/core/valveselfconfigure/one-node', params)
        // note: this seems to be logged multiple times
        reposeLogSearch.awaitByString(
            "Configuration Updated: SystemModel",
            3,
            35,
            TimeUnit.SECONDS
        )

        then:
        1 == 1

        when: "Repose reloads the configs"
        def mc = deproxy.makeRequest(url: "http://localhost:${port}")
        then: "the first node should be available"
        mc.receivedResponse.code == "200"
        mc.handlings.size() == 1
    }
}
