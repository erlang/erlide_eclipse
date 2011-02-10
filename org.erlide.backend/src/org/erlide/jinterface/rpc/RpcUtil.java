/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.jinterface.rpc;

import org.erlide.jinterface.util.ErlLogger;
import org.erlide.jinterface.util.TypeConverter;

import com.ericsson.otp.erlang.OtpErlang;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.erlang.Signature;
import com.ericsson.otp.erlang.SignatureException;

public final class RpcUtil {
    public static final int INFINITY = -1;

    // use this for debugging
    private static final boolean CHECK_RPC = Boolean
            .getBoolean("org.erlide.checkrpc");

    /**
     * Convenience method to send a remote message.
     * 
     * @param node
     * @param pid
     * @param msg
     * @throws RpcException
     */
    public static void send(final OtpNode node, final OtpErlangPid pid,
            final Object msg) throws SignatureException {
        final OtpMbox mbox = node.createMbox();
        try {
            if (mbox != null) {
                if (CHECK_RPC) {
                    debug("SEND " + pid + "-> " + msg);
                }
                mbox.send(pid, TypeConverter.java2erlang(msg, "x"));
            }
        } finally {
            node.closeMbox(mbox);
        }
    }

    /**
     * Convenience method to send a remote message.
     * 
     * @param node
     * @param peer
     * @param name
     * @param msg
     * @throws RpcException
     */
    public static void send(final OtpNode node, final String peer,
            final String name, final Object msg) throws SignatureException {
        final OtpMbox mbox = node.createMbox();
        try {
            if (mbox != null) {
                if (CHECK_RPC) {
                    debug("SEND " + name + "-> " + msg);
                }
                mbox.send(name, peer, TypeConverter.java2erlang(msg, "x"));
            }
        } finally {
            node.closeMbox(mbox);
        }
    }

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
    public static OtpErlangObject rpcCall(final OtpNode node,
            final String peer, final boolean logCalls,
            final OtpErlangObject gleader, final String module,
            final String fun, final int timeout, final String signature,
            final Object... args0) throws RpcException, SignatureException {
        final RpcFuture future = sendRpcCall(node, peer, logCalls, gleader,
                module, fun, signature, args0);
        OtpErlangObject result;
        result = future.get(timeout);
        if (CHECK_RPC) {
            debug("RPC result:: " + result);
        }
        if (isBadRpc(result)) {
            throw new RpcException(result.toString());
        }
        return result;
    }

    public static boolean isBadRpc(final OtpErlangObject result) {
        if (result instanceof OtpErlangTuple) {
            final OtpErlangTuple t = (OtpErlangTuple) result;
            if (t.elementAt(0) instanceof OtpErlangAtom) {
                final OtpErlangAtom a = (OtpErlangAtom) t.elementAt(0);
                return "badrpc".equals(a.atomValue());
            }
        }
        return false;
    }

    /**
     * Calls a function that supports sending progress reports back. The first
     * argument is implicit and is the pid where the reports are to be sent.
     */
    public static void rpcCallWithProgress(final RpcResultCallback callback,
            final OtpNode node, final String peer, final boolean logCalls,
            final OtpErlangObject gleader, final String module,
            final String fun, final int timeout, final String signature,
            final Object... args0) throws SignatureException {
        final Object[] args = new Object[args0.length + 1];
        System.arraycopy(args0, 0, args, 1, args0.length);
        final OtpMbox mbox = node.createMbox();
        args[0] = mbox.self();
        new RpcResultReceiver(mbox, callback);
        rpcCast(node, peer, logCalls, gleader, module, fun, signature, args);
    }

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
    public static synchronized RpcFuture sendRpcCall(final OtpNode node,
            final String peer, final boolean logCalls,
            final OtpErlangObject gleader, final String module,
            final String fun, final String signature, final Object... args0)
            throws SignatureException {
        final OtpErlangObject[] args = convertArgs(signature, args0);

        OtpErlangObject res = null;
        final OtpMbox mbox = node.createMbox();
        res = RpcUtil.buildRpcCall(mbox.self(), gleader, module, fun, args);
        if (logCalls) {
            debugLogCallArgs("call -> %s:%s(%s)", module, fun, argString(args));
        }
        mbox.send("rex", peer, res);
        if (CHECK_RPC) {
            debug("RPC " + mbox.hashCode() + "=> " + res);
        }
        return new RpcFuture(mbox, module + ":" + fun + "/" + args0.length,
                logCalls);
    }

    final static String SEP = ", ";

    private static Object argString(final OtpErlangObject[] args) {
        final StringBuilder result = new StringBuilder();
        for (final OtpErlangObject arg : args) {
            final String s = arg.toString();
            result.append(s).append(SEP);
        }
        final String r = result.length() == 0 ? "" : result.substring(0,
                result.length() - SEP.length());
        return r;
    }

    /**
     * Retrieve the result of a RPC.
     * 
     * @param mbox
     * @return
     * @throws RpcException
     */
    public static OtpErlangObject getRpcResult(final OtpMbox mbox,
            final String env) throws RpcException {
        return getRpcResult(mbox, INFINITY, env);
    }

    /**
     * Retrieve the result of a RPC.
     * 
     * @param mbox
     * @param timeout
     * @param env
     * @return
     * @throws RpcException
     */
    public static OtpErlangObject getRpcResult(final OtpMbox mbox,
            final long timeout, final String env) throws RpcException {
        assert mbox != null;

        OtpErlangObject res = null;
        try {
            try {
                if (timeout == INFINITY) {
                    res = mbox.receive();
                } else {
                    res = mbox.receive(timeout);
                }
                if (CHECK_RPC) {
                    debug("RPC " + mbox.hashCode() + "<= " + res);
                }
            } finally {
                if (res != null) {
                    mbox.close();
                }
            }
            if (res == null) {
                final String msg = env != null ? env : "??";
                throw new RpcTimeoutException(msg);
            }
            if (!(res instanceof OtpErlangTuple)) {
                throw new RpcException(res.toString());
            }
            final OtpErlangTuple t = (OtpErlangTuple) res;
            if (t.arity() != 2) {
                throw new RpcException(res.toString());
            }
            res = t.elementAt(1);

        } catch (final OtpErlangExit e) {
            throw new RpcException(e);
        } catch (final OtpErlangDecodeException e) {
            throw new RpcException(e);
        }
        return res;
    }

    private static OtpErlangObject buildRpcCall(final OtpErlangPid pid,
            final OtpErlangObject gleader, final String module,
            final String fun, final OtpErlangObject[] args) {
        final OtpErlangObject m = new OtpErlangAtom(module);
        final OtpErlangObject f = new OtpErlangAtom(fun);
        final OtpErlangObject a = new OtpErlangList(args);
        return OtpErlang.mkTuple(pid,
                OtpErlang.mkTuple(new OtpErlangAtom("call"), m, f, a, gleader));
    }

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
    public static void rpcCast(final OtpNode node, final String peer,
            final boolean logCalls, final OtpErlangObject gleader,
            final String module, final String fun, final String signature,
            final Object... args0) throws SignatureException {
        final OtpErlangObject[] args = convertArgs(signature, args0);

        OtpErlangObject res = null;
        res = RpcUtil.buildRpcCastMsg(gleader, module, fun, args);
        if (logCalls) {
            debugLogCallArgs("cast -> %s:%s(%s)", module, fun, argString(args));
        }
        RpcUtil.send(node, peer, "rex", res);
        if (CHECK_RPC) {
            debug("RPC _cast_" + "=> " + res);
        }
    }

    public static void debugLogCallArgs(final String fmt, final Object... args0) {
        ErlLogger.debug(fmt, args0);
    }

    private static OtpErlangObject[] convertArgs(final String signature,
            Object... args0) throws SignatureException {
        if (args0 == null) {
            args0 = new OtpErlangObject[] {};
        }

        Signature[] type;
        type = Signature.parse(signature);
        if (type == null) {
            type = new Signature[args0.length];
            for (int i = 0; i < args0.length; i++) {
                type[i] = new Signature('x');
            }
        }
        if (type.length != args0.length) {
            throw new SignatureException(
                    "Signature doesn't match parameter number: " + type.length
                            + "/" + args0.length);
        }
        final OtpErlangObject[] args = new OtpErlangObject[args0.length];
        for (int i = 0; i < args.length; i++) {
            args[i] = TypeConverter.java2erlang(args0[i], type[i]);
        }
        return args;
    }

    private static OtpErlangObject buildRpcCastMsg(
            final OtpErlangObject gleader, final String module,
            final String fun, final OtpErlangObject[] args) {
        final OtpErlangObject m = new OtpErlangAtom(module);
        final OtpErlangObject f = new OtpErlangAtom(fun);
        final OtpErlangObject a = new OtpErlangList(args);
        final OtpErlangAtom castTag = new OtpErlangAtom("$gen_cast");
        return OtpErlang.mkTuple(castTag,
                OtpErlang.mkTuple(new OtpErlangAtom("cast"), m, f, a, gleader));
    }

    private static void debug(final String s) {
        ErlLogger.debug(s);
    }

    @SuppressWarnings("unused")
    private static void warn(final Exception e) {
        ErlLogger.debug(e);
    }

    private RpcUtil() {
    }
}
