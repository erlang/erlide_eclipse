package org.erlide.jinterface;

import org.erlide.jinterface.rpc.IRpcCallback;
import org.erlide.jinterface.rpc.IRpcFuture;
import org.erlide.jinterface.rpc.IRpcResultCallback;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.RpcResult;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public interface IRpcSite {

    /**
     * typed RPC
     * 
     */
    public abstract RpcResult call_noexception(final String m, final String f,
            final String signature, final Object... a);

    /**
     * typed RPC with timeout
     * 
     * @throws ConversionException
     */
    public abstract RpcResult call_noexception(final int timeout,
            final String m, final String f, final String signature,
            final Object... args);

    public abstract IRpcFuture async_call(final String m, final String f,
            final String signature, final Object... args) throws RpcException;

    public abstract void async_call_cb(final IRpcCallback cb, final String m,
            final String f, final String signature, final Object... args)
            throws RpcException;

    public abstract void cast(final String m, final String f,
            final String signature, final Object... args) throws RpcException;

    public abstract OtpErlangObject call(final String m, final String f,
            final String signature, final Object... a) throws RpcException;

    public abstract OtpErlangObject call(final int timeout, final String m,
            final String f, final String signature, final Object... a)
            throws RpcException;

    public abstract OtpErlangObject call(final int timeout,
            final OtpErlangObject gleader, final String m, final String f,
            final String signature, final Object... a) throws RpcException;

    public abstract void async_call_result(final IRpcResultCallback cb,
            final String m, final String f, final String signature,
            final Object... args) throws RpcException;

    public abstract void send(final OtpErlangPid pid, final Object msg);

    public abstract void send(final String name, final Object msg);

}
