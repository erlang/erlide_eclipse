package com.ericsson.otp.erlang;

public class OtpErlangPidProxy extends OtpErlangPid {

    private static final long serialVersionUID = 1L;

    private final String node;

    public OtpErlangPidProxy(final String node, final OtpInputStream buf)
            throws OtpErlangDecodeException {
        super(buf);
        this.node = node;
    }

    @Override
    public String toString() {
        if (node.equals(node())) {
            return "#Pid<0." + id() + "." + serial() + ">";
        } else {
            return super.toString();
        }
    }

    @Override
    public boolean equals(final Object o) {
        return super.equals(o);
    }

    @Override
    public int hashCode() {
        return super.hashCode();
    }
}
