package org.erlide.core.rpc;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.erlang.SignatureException;

public interface IRpcHelper {

    public static final int INFINITY = -1;

    /**
     * Convenience method to send a remote message.
     * 
     * @param node
     * @param pid
     * @param msg
     * @throws RpcException
     */
    void send(final OtpNode node, final OtpErlangPid pid, final Object msg)
            throws SignatureException;

    /**
     * Convenience method to send a remote message.
     * 
     * @param node
     * @param peer
     * @param name
     * @param msg
     * @throws RpcException
     */
    void send(final OtpNode node, final String peer, final String name,
            final Object msg) throws SignatureException;

    /**
     * Make a regular RPC to the given node, with the given arguments.
     * 
     * @param node
     * @param peer
     * @param module
     * @param fun
     * @param timeout
     * @param signature
     * @param args0
     * @return
     * @throws RpcException
     */
    OtpErlangObject rpcCall(final OtpNode node, final String peer,
            final boolean logCalls, final OtpErlangObject gleader,
            final String module, final String fun, final int timeout,
            final String signature, final Object... args0) throws RpcException,
            SignatureException;

    boolean isBadRpc(final OtpErlangObject result);

    /**
     * Calls a function that supports sending progress reports back. The first
     * argument is implicit and is the pid where the reports are to be sent.
     */
    void rpcCastWithProgress(final IRpcResultCallback callback,
            final OtpNode node, final String peer, final boolean logCalls,
            final OtpErlangObject gleader, final String module,
            final String fun, final String signature, final Object... args0)
            throws SignatureException;

    /**
     * Send a RPC request and return the mailbox that will receive the result
     * once it's delivered.
     * 
     * @param node
     * @param peer
     * @param module
     * @param fun
     * @param signature
     * @param args0
     * @return
     * @throws RpcException
     */
    IRpcFuture sendRpcCall(final OtpNode node, final String peer,
            final boolean logCalls, final OtpErlangObject gleader,
            final String module, final String fun, final String signature,
            final Object... args0) throws SignatureException;

    /**
     * Retrieve the result of a RPC.
     * 
     * @param mbox
     * @return
     * @throws RpcException
     */
    OtpErlangObject getRpcResult(final OtpMbox mbox, final String env)
            throws RpcException;

    /**
     * Retrieve the result of a RPC.
     * 
     * @param mbox
     * @param timeout
     * @param env
     * @return
     * @throws RpcException
     */
    OtpErlangObject getRpcResult(final OtpMbox mbox, final long timeout,
            final String env) throws RpcException;

    /**
     * Make a RPC but don't wait for any result.
     * 
     * @param node
     * @param peer
     * @param logCalls
     * @param module
     * @param fun
     * @param signature
     * @param args0
     * @throws RpcException
     */
    void rpcCast(final OtpNode node, final String peer, final boolean logCalls,
            final OtpErlangObject gleader, final String module,
            final String fun, final String signature, final Object... args0)
            throws SignatureException;

    void debugLogCallArgs(final String fmt, final Object... args0);

    void makeAsyncCbCall(final OtpNode node, final String peer,
            final IRpcCallback cb, final int timeout,
            final OtpErlangObject gleader, final String module,
            final String fun, final String signature, final Object... args)
            throws SignatureException;

}
