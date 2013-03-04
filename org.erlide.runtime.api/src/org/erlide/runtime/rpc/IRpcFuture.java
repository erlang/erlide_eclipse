package org.erlide.runtime.rpc;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface IRpcFuture {

    public abstract OtpErlangObject get() throws RpcException;

    public abstract OtpErlangObject get(final long timeout) throws RpcException;

    public abstract boolean isDone();

}
