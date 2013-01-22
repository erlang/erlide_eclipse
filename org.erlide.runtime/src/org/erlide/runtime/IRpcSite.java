package org.erlide.runtime;

import org.erlide.runtime.rpc.IRpcCallback;
import org.erlide.runtime.rpc.IRpcFuture;
import org.erlide.runtime.rpc.IRpcResultCallback;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.rpc.RpcResult;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public interface IRpcSite {

    /**
     * typed RPC
     * 
     */
    RpcResult call_noexception(final String m, final String f,
            final String signature, final Object... a);

    /**
     * typed RPC with timeout
     * 
     * @throws ConversionException
     */
    RpcResult call_noexception(final int timeout, final String m,
            final String f, final String signature, final Object... args);

    IRpcFuture async_call(final String m, final String f,
            final String signature, final Object... args) throws RpcException;

    IRpcFuture async_call(final OtpErlangObject gleader, final String m,
            final String f, final String signature, final Object... args)
            throws RpcException;

    void async_call_cb(final IRpcCallback cb, final String m, final String f,
            final String signature, final Object... args) throws RpcException;

    void async_call_cb(final IRpcCallback cb, final int timeout,
            final String m, final String f, final String signature,
            final Object... args) throws RpcException;

    void async_call_cb(final IRpcCallback cb, final int timeout,
            final OtpErlangObject gleader, final String m, final String f,
            final String signature, final Object... args) throws RpcException;

    void cast(final String m, final String f, final String signature,
            final Object... args) throws RpcException;

    void cast(final OtpErlangObject gleader, final String m, final String f,
            final String signature, final Object... args) throws RpcException;

    OtpErlangObject call(final String m, final String f,
            final String signature, final Object... a) throws RpcException;

    OtpErlangObject call(final int timeout, final String m, final String f,
            final String signature, final Object... a) throws RpcException;

    OtpErlangObject call(final int timeout, final OtpErlangObject gleader,
            final String m, final String f, final String signature,
            final Object... a) throws RpcException;

    void async_call_result(final IRpcResultCallback cb, final String m,
            final String f, final String signature, final Object... args)
            throws RpcException;

    void send(final OtpErlangPid pid, final Object msg);

    void send(final String name, final Object msg);

    void send(final String fullNodeName, final String name, final Object msg);

}
