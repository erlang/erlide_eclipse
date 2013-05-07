package org.erlide.backend.api;

import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.RuntimeVersion;


public interface IBackendProvider {

    IRpcSite get();

    IRpcSite get(RuntimeVersion version);

    IRpcSite get(String project);

}
