package org.erlide.tracing.core;

import com.ericsson.otp.erlang.OtpErlangAtom;

/**
 * Enum describing ways in which processes can be traced.
 * 
 * @author Piotr Dorobisz
 * 
 */
public enum ProcessMode {
    //@formatter:off
    ALL("all (existing + new)", new OtpErlangAtom("all")), 
    NEW("new", new OtpErlangAtom("new")), 
    EXISTING("existing", new OtpErlangAtom("existing")), 
    BY_PID("selected", null);
    //@formatter:on

    private OtpErlangAtom atom;
    private String name;

    private ProcessMode(final String name, final OtpErlangAtom atom) {
        this.name = name;
        this.atom = atom;
    }

    public OtpErlangAtom toAtom() {
        return atom;
    }

    public String getName() {
        return name;
    }
}
