package org.ttb.integration;

import com.ericsson.otp.erlang.OtpErlangAtom;

/**
 * Enum containing flags which may be set on process under tracing.
 * 
 * @author Piotr Dorobisz
 * 
 */
public enum ProcessFlag {

    // TODO add all flags
    SEND("send", new OtpErlangAtom("s")), //
    RECEIVE("receive", new OtpErlangAtom("r")), //
    CALL("call", new OtpErlangAtom("c")), //
    PROCS("procs", new OtpErlangAtom("p"));

    private OtpErlangAtom atom;
    private String name;

    private ProcessFlag(String name, OtpErlangAtom atom) {
        this.name = name;
        this.atom = atom;
    }

    public String getName() {
        return name;
    }

    public OtpErlangAtom toAtom() {
        return atom;
    }
}
