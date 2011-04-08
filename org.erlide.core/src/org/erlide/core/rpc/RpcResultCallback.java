package org.erlide.core.rpc;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface RpcResultCallback {

    public abstract void start(final OtpErlangObject msg);

    public abstract void stop(final OtpErlangObject msg);

    public abstract void progress(final OtpErlangObject msg);

}
