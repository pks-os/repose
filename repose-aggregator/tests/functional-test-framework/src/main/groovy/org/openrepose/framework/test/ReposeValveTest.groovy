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
package org.openrepose.framework.test

import org.apache.commons.io.FileUtils
import org.linkedin.util.clock.SystemClock
import org.rackspace.deproxy.Deproxy
import org.rackspace.deproxy.MessageChain
import spock.lang.Shared
import spock.lang.Specification

import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException

import static org.linkedin.groovy.util.concurrent.GroovyConcurrentUtils.waitForCondition
import static org.openrepose.framework.test.ReposeLauncher.MAX_STARTUP_TIME

abstract class ReposeValveTest extends Specification {

    /**
     * Used to get the JMX hostname when needing to resolve JMX stuff
     */
    @Shared
    String jmxHostname = InetAddress.getLocalHost().getHostName()

    @Shared
    def ReposeValveLauncher repose

    @Shared
    def Deproxy deproxy

    @Shared
    def TestProperties properties

    @Shared
    def ReposeLogSearch reposeLogSearch

    @Shared
    int clientId = 0

    def setupSpec() {

        String className = this.getClass().canonicalName
        properties = new TestProperties(className.replace('.', '/'))

        configureReposeValve()
        repose.configurationProvider.cleanConfigDirectory()
    }

    def configureReposeValve() {

        ReposeConfigurationProvider reposeConfigProvider = new ReposeConfigurationProvider(properties)

        repose = new ReposeValveLauncher(reposeConfigProvider, properties)
        repose.enableDebug()
        reposeLogSearch = new ReposeLogSearch(logFile);
    }

    def cleanupSpec() {
        deproxy?.shutdown()
        repose?.stop()
    }

    def cleanLogDirectory() {
        FileUtils.deleteQuietly(new File(logFile))
    }

    /**
     * This needs to be the default way to determine if repose is ready to serve requests I think...
     * @param responseCode
     * @param throwException
     * @param checkLogMessage
     * @return
     */
    def waitUntilReadyToServiceRequests(String responseCode = '200',
                                        boolean throwException = true,
                                        boolean checkLogMessage = false) {
        def clock = new SystemClock()
        def innerDeproxy = new Deproxy()
        def logSearch = new ReposeLogSearch(properties.logFile)
        if (checkLogMessage)
            logSearch.cleanLog()
        MessageChain mc
        try {
            waitForCondition(clock, "${MAX_STARTUP_TIME}s", '1s', {
                if (checkLogMessage &&
                        // WARNING: This check is not node aware; if any node is ready, all of Repose is considered ready.
                        logSearch.awaitByString(
                                "Repose ready", 1, MAX_STARTUP_TIME, TimeUnit.SECONDS).size() > 0) {
                    return true
                }
                try {
                    mc = innerDeproxy.makeRequest(url: reposeEndpoint)
                } catch (Exception e) {
                }
                if (mc != null) {
                    println mc.receivedResponse.code
                    return mc.receivedResponse.code.equals(responseCode)
                } else {
                    return false
                }
            })
        } catch (TimeoutException exc) {
            if (throwException) {
                throw exc
            } else {
                return false
            }
        }

    }

    def waitForHttpClientRequestCacheToClear(long timeoutMilliseconds = 500) {
        // Some tests reuse a single repose process across multiple test methods.
        // In these tests, the repose http client will cache requests from one test
        // method into the next test method.
        //
        // Each test _may_ have a different setting for the Repose client cache,
        // but this is typically only 500ms.
        sleep timeoutMilliseconds
    }

    // Helper methods to minimize refactoring in all test classes
    def getReposeEndpoint() {
        return properties.getReposeEndpoint()
    }

    def getConnFramework() {
        return properties.getConnFramework()
    }

    def getConfigTemplates() {
        return properties.getConfigTemplates()
    }

    def getConfigDirectory() {
        return properties.getConfigDirectory()
    }

    def getLogFile() {
        return properties.getLogFile()
    }

    def getReposeContainer() {
        return properties.getReposeContainer()
    }


}
