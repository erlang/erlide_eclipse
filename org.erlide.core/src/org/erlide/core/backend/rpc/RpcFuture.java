package org.erlide.core.backend.rpc;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface RpcFuture {

    public abstract OtpErlangObject get() throws RpcException;

    public abstract OtpErlangObject get(final long timeout) throws RpcException;

    public abstract boolean isDone();

}
