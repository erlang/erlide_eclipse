package org.erlide.tracing.core.mvc.model;

import java.io.Serializable;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * Match specification for tracing pattern.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class MatchSpec implements Serializable {

    private static final long serialVersionUID = -251703821338799161L;

    private String functionString = "";
    private OtpErlangObject msObject;

    @Override
    public boolean equals(final Object o) {
        if (o == null || !o.getClass().equals(MatchSpec.class)) {
            return false;
        }
        return functionString.equals(((MatchSpec) o).functionString);
    }

    @Override
    public int hashCode() {
        if (functionString == null) {
            return 0;
        } else {
            return functionString.hashCode();
        }
    }

    public String getFunctionString() {
        return functionString;
    }

    public void setFunctionString(final String functionString) {
        this.functionString = functionString;
    }

    public OtpErlangObject getMsObject() {
        return msObject;
    }

    public void setMsObject(final OtpErlangObject msObject) {
        this.msObject = msObject;
    }

    @Override
    public String toString() {
        return functionString;
    }
}
