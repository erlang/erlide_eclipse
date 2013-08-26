package org.erlide.runtime.api;

import org.erlide.runtime.runtimeinfo.RuntimeVersion;

public interface IRpcSiteProvider {

    IRpcSite get();

    IRpcSite get(RuntimeVersion version);

    IRpcSite get(String projectName);

}
