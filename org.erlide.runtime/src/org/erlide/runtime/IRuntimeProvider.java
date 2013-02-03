package org.erlide.runtime;

import com.ericsson.otp.erlang.RuntimeVersion;

public interface IRuntimeProvider {

    IRpcSite get();

    IRpcSite get(RuntimeVersion version);

}
