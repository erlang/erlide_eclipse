package org.erlide.core.services.search;

import com.ericsson.otp.erlang.OtpErlangList;

public class ErlOpenContext {
    private final String externalModules;
    private final OtpErlangList pathVars;

    public ErlOpenContext(final String externalModules,
            final OtpErlangList pathVars) {
        this.externalModules = externalModules;
        this.pathVars = pathVars;
    }

    public String getExternalModules() {
        return externalModules;
    }

    public OtpErlangList getPathVars() {
        return pathVars;
    }
}
