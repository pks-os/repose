package com.rackspace.papi.service.management;

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: Oct 24, 2012
 * Time: 3:24:42 PM
 */
public interface ManagementService {

    public void start(int managementPort, String artifactDirectory, String managementContext);
    public void stop();
}
