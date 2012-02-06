package org.erlide.tracing.core;

import com.ericsson.otp.erlang.OtpErlangAtom;

/**
 * Enum containing flags which may be set on process under tracing.
 * 
 * @author Piotr Dorobisz
 * 
 */
public enum ProcessFlag {

    //@formatter:off
    CALL("call", new OtpErlangAtom("c")), 
    PROCS("procs", new OtpErlangAtom("p")), 
    RECEIVE("receive", new OtpErlangAtom("r")), 
    SEND("send", new OtpErlangAtom("s")), 
    SOFS("set on first spawn", new OtpErlangAtom("sofs")), 
    SOFL("set on first link", new OtpErlangAtom("sofl")), 
    SOL("set on link", new OtpErlangAtom("sol")), 
    SOS("set on spawn", new OtpErlangAtom("sos"));
    //@formatter:on

    private OtpErlangAtom atom;
    private String name;

    private ProcessFlag(final String name, final OtpErlangAtom atom) {
        this.name = name;
        this.atom = atom;
    }

    /**
     * Returns enum value for given ordinal. If there is no enum with given
     * ordinal it will return <code>null</code>.
     * 
     * @param index
     * @return enum value
     */
    public static ProcessFlag getByIndex(final int index) {
        for (final ProcessFlag column : ProcessFlag.values()) {
            if (column.ordinal() == index) {
                return column;
            }
        }
        return null;
    }

    public String getName() {
        return name;
    }

    public OtpErlangAtom toAtom() {
        return atom;
    }
}
