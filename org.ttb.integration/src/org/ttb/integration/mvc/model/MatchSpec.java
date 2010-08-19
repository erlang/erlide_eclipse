package org.ttb.integration.mvc.model;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * Match specification for tracing pattern.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class MatchSpec {
    private String functionString;
    private OtpErlangObject msObject;

    public String getFunctionString() {
        return functionString;
    }

    public void setFunctionString(String functionString) {
        this.functionString = functionString;
    }

    public OtpErlangObject getMsObject() {
        return msObject;
    }

    public void setMsObject(OtpErlangObject msObject) {
        this.msObject = msObject;
    }

    @Override
    public String toString() {
        return functionString;
    }
}
