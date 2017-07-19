package org.erlide.util.erlang;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpInputStream;

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
        }
        return super.toString();
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
