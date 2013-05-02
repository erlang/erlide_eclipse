package org.erlide.runtime;

import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.RuntimeVersion;

public interface IRuntimeProvider {

    IRpcSite get();

    IRpcSite get(RuntimeVersion version);

    IRpcSite get(String project);

}
