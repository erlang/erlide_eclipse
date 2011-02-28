package org.erlide.core.backend.rpc;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface RpcResult {

    public abstract boolean isOk();

    public abstract OtpErlangObject getValue();

    public abstract String toString();

}
