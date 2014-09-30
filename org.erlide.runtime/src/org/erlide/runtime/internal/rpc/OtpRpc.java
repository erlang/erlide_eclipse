package org.erlide.runtime.internal.rpc;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.erlide.runtime.api.IOtpNodeProxy;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.rpc.IRpcCallback;
import org.erlide.runtime.rpc.IRpcResultCallback;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.rpc.RpcFuture;
import org.erlide.runtime.rpc.RpcMonitor;
import org.erlide.runtime.rpc.RpcResult;
import org.erlide.runtime.rpc.RpcTimeoutException;
import org.erlide.util.ErlLogger;
import org.erlide.util.erlang.OtpErlang;
import org.erlide.util.erlang.Signature;
import org.erlide.util.erlang.SignatureException;
import org.erlide.util.erlang.TypeConverter;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import com.google.common.util.concurrent.ThreadFactoryBuilder;

public class OtpRpc implements IOtpRpc {

    private static final OtpErlangAtom USER_ATOM = new OtpErlangAtom("user");

    public static final long INFINITY = Long.MAX_VALUE;

    // use this for debugging
    public static final boolean CHECK_RPC = Boolean.getBoolean("erlide.checkrpc");

    public static long DEFAULT_TIMEOUT;
    {
        setDefaultTimeout();
    }

    private static final ThreadFactory threadFactory = new ThreadFactoryBuilder()
            .setDaemon(true).setNameFormat("rpc-%d").build();
    private static final ExecutorService threadPool = Executors
            .newCachedThreadPool(threadFactory);

    private final IOtpNodeProxy runtime;
    private final String nodeName;
    private final OtpNode localNode;
    private volatile boolean connected;

    public OtpRpc(final IOtpNodeProxy runtime, final OtpNode localNode,
            final String nodeName) {
        this.runtime = runtime;
        this.localNode = localNode;
        this.nodeName = nodeName;
        connected = false;
    }

    @Override
    public void setConnected(final boolean connected) {
        this.connected = connected;
    }

    @Override
    public void async_call_result(final IRpcResultCallback cb, final String m,
            final String f, final String signature, final Object... args)
            throws RpcException {
        final OtpErlangAtom gleader = USER_ATOM;
        try {
            final Object[] args1 = new Object[args.length + 1];
            System.arraycopy(args, 0, args1, 1, args.length);
            final OtpMbox mbox = localNode.createMbox();
            args1[0] = mbox.self();
            new RpcResultReceiver(mbox, cb);
            rpcCast(localNode, nodeName, false, gleader, m, f, signature, args1);
        } catch (final SignatureException e) {
            throw new RpcException(e);
        }
    }

    @Override
    public RpcFuture async_call(final OtpErlangObject gleader, final String module,
            final String fun, final String signature, final Object... args0)
            throws RpcException {
        checkConnected();
        try {
            return sendRpcCall(localNode, nodeName, false, gleader, module, fun,
                    signature, args0);
        } catch (final SignatureException e) {
            throw new RpcException(e);
        }
    }

    @Override
    public RpcFuture async_call(final String module, final String fun,
            final String signature, final Object... args0) throws RpcException {
        return async_call(USER_ATOM, module, fun, signature, args0);
    }

    @Override
    public void async_call_cb(final IRpcCallback cb, final long timeout,
            final String module, final String fun, final String signature,
            final Object... args) throws RpcException {
        async_call_cb(cb, timeout, USER_ATOM, module, fun, signature, args);
    }

    @Override
    public void async_call_cb(final IRpcCallback cb, final long timeout,
            final OtpErlangObject gleader, final String module, final String fun,
            final String signature, final Object... args) throws RpcException {
        checkConnected();
        try {
            final RpcFuture future = sendRpcCall(localNode, nodeName, false, gleader,
                    module, fun, signature, args);
            final Runnable target = new Runnable() {
                @Override
                public void run() {
                    OtpErlangObject result;
                    try {
                        result = future.checkedGet(timeout, TimeUnit.MILLISECONDS);
                        cb.onSuccess(result);
                    } catch (final Exception e) {
                        ErlLogger.error("Could not execute RPC " + module + ":" + fun
                                + " : " + e.getMessage());
                        cb.onFailure(e);
                    }
                }
            };
            // We can't use jobs here, it's an Eclipse dependency
            threadPool.execute(target);
        } catch (final SignatureException e) {
            throw new RpcException(e);
        }
    }

    @Override
    public OtpErlangObject call(final long timeout, final OtpErlangObject gleader,
            final String module, final String fun, final String signature,
            final Object... args0) throws RpcException {
        checkConnected();
        OtpErlangObject result = null;
        try {
            final RpcFuture future = sendRpcCall(localNode, nodeName, false, gleader,
                    module, fun, signature, args0);
            result = future.checkedGet(timeout, TimeUnit.MILLISECONDS);
            if (CHECK_RPC) {
                ErlLogger.debug("RPC result:: " + result);
            }
            if (isBadRpc(result)) {
                throw new RpcException("Bad RPC: " + result);
            }
        } catch (final SignatureException e) {
            throw new RpcException(e);
        } catch (final TimeoutException e) {
            throw new RpcTimeoutException(e.getMessage());
        }
        return result;
    }

    @Override
    public OtpErlangObject call(final long timeout, final String module,
            final String fun, final String signature, final Object... args0)
            throws RpcException {
        return call(timeout, USER_ATOM, module, fun, signature, args0);
    }

    @Override
    public void cast(final OtpErlangObject gleader, final String module,
            final String fun, final String signature, final Object... args0)
            throws RpcException {
        checkConnected();
        try {
            rpcCast(localNode, nodeName, false, gleader, module, fun, signature, args0);
        } catch (final SignatureException e) {
            throw new RpcException(e);
        }
    }

    @Override
    public void cast(final String module, final String fun, final String signature,
            final Object... args0) throws RpcException {
        cast(USER_ATOM, module, fun, signature, args0);
    }

    @Override
    public void send(final OtpErlangPid pid, final Object msg) {
        try {
            checkConnected();
            final OtpMbox mbox = localNode.createMbox();
            try {
                if (mbox != null) {
                    if (CHECK_RPC) {
                        ErlLogger.debug("SEND " + pid + "-> " + msg);
                    }
                    mbox.send(pid, TypeConverter.java2erlang(msg, "x"));
                }
            } finally {
                localNode.closeMbox(mbox);
            }
        } catch (final Exception e) {
        }
    }

    @Override
    public void send(final String fullNodeName, final String name, final Object msg) {
        try {
            checkConnected();
            send(localNode, fullNodeName, name, msg);
        } catch (final Exception e) {
        }
    }

    @Override
    public RpcResult call_noexception(final String m, final String f,
            final String signature, final Object... args) {
        return call_noexception(DEFAULT_TIMEOUT, m, f, signature, args);
    }

    @Override
    public RpcResult call_noexception(final long timeout, final String m, final String f,
            final String signature, final Object... args) {
        try {
            final OtpErlangObject result = call(timeout, m, f, signature, args);
            return new RpcResult(result);
        } catch (final RpcException e) {
            return RpcResult.error(e.getMessage());
        }
    }

    @Override
    public void async_call_cb(final IRpcCallback cb, final String m, final String f,
            final String signature, final Object... args) throws RpcException {
        async_call_cb(cb, DEFAULT_TIMEOUT, m, f, signature, args);
    }

    @Override
    public OtpErlangObject call(final String m, final String f, final String signature,
            final Object... a) throws RpcException {
        return call(DEFAULT_TIMEOUT, m, f, signature, a);
    }

    @Override
    public void send(final String name, final Object msg) {
        try {
            checkConnected();
            send(localNode, nodeName, name, msg);
        } catch (final Exception e) {
        }
    }

    private void checkConnected() throws RpcException {
        if (!isConnected()) {
            throw new RpcException(
                    String.format("backend %s down", runtime.getNodeName()));
        }
    }

    public boolean isConnected() {
        return connected;
    }

    private static void setDefaultTimeout() {
        final String t = System.getProperty("erlide.rpc.timeout", "9000");
        if ("infinity".equals(t)) {
            DEFAULT_TIMEOUT = INFINITY;
        } else {
            try {
                DEFAULT_TIMEOUT = Integer.parseInt(t);
            } catch (final Exception e) {
                DEFAULT_TIMEOUT = 9000;
            }
        }
    }

    private void send(final OtpNode node, final String peer, final String name,
            final Object msg) throws SignatureException {
        final OtpMbox mbox = node.createMbox();
        try {
            if (mbox != null) {
                if (CHECK_RPC) {
                    ErlLogger.debug("SEND " + name + "-> " + msg);
                }
                mbox.send(name, peer, TypeConverter.java2erlang(msg, "x"));
            }
        } finally {
            node.closeMbox(mbox);
        }
    }

    private boolean isBadRpc(final OtpErlangObject result) {
        if (result instanceof OtpErlangTuple) {
            final OtpErlangTuple t = (OtpErlangTuple) result;
            if (t.elementAt(0) instanceof OtpErlangAtom) {
                final OtpErlangAtom a = (OtpErlangAtom) t.elementAt(0);
                return "badrpc".equals(a.atomValue());
            }
        }
        return false;
    }

    private synchronized RpcFuture sendRpcCall(final OtpNode node, final String peer,
            final boolean logCalls, final OtpErlangObject gleader, final String module,
            final String fun, final String signature, final Object... args0)
            throws SignatureException {
        final OtpErlangObject[] args = convertArgs(signature, args0);

        OtpErlangObject res = null;
        final OtpMbox mbox = node.createMbox();
        res = buildRpcCall(mbox.self(), gleader, module, fun, args);
        if (logCalls) {
            final Object[] args01 = { module, fun, argString(args) };
            ErlLogger.debug("call -> %s:%s(%s)", args01);
        }
        //
        final OtpErlangRef ref = RpcMonitor.recordRequest(node, peer, module, fun, args,
                OtpErlang.sizeOf(res));
        //
        mbox.send("rex", peer, res);
        if (CHECK_RPC) {
            ErlLogger.debug("RPC " + mbox.hashCode() + "=> " + res);
        }
        return new RpcFuture(ref, mbox, module + ":" + fun + "/" + args0.length,
                logCalls, this);
    }

    private static final String SEP = ", ";

    private Object argString(final OtpErlangObject[] args) {
        final StringBuilder result = new StringBuilder();
        for (final OtpErlangObject arg : args) {
            final String s = arg.toString();
            result.append(s).append(SEP);
        }
        final String r = result.length() == 0 ? "" : result.substring(0, result.length()
                - SEP.length());
        return r;
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
    @Override
    public OtpErlangObject getRpcResult(final OtpMbox mbox, final long timeout,
            final String env) throws RpcException {
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
                    ErlLogger.debug("RPC " + mbox.hashCode() + "<= " + res);
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

    private OtpErlangObject buildRpcCall(final OtpErlangPid pid,
            final OtpErlangObject gleader, final String module, final String fun,
            final OtpErlangObject[] args) {
        final OtpErlangObject m = new OtpErlangAtom(module);
        final OtpErlangObject f = new OtpErlangAtom(fun);
        final OtpErlangObject a = new OtpErlangList(args);
        return OtpErlang.mkTuple(pid,
                OtpErlang.mkTuple(new OtpErlangAtom("call"), m, f, a, gleader));
    }

    private void rpcCast(final OtpNode node, final String peer, final boolean logCalls,
            final OtpErlangObject gleader, final String module, final String fun,
            final String signature, final Object... args0) throws SignatureException {
        final OtpErlangObject[] args = convertArgs(signature, args0);

        OtpErlangObject msg = null;
        msg = buildRpcCastMsg(gleader, module, fun, args);
        if (logCalls) {
            final Object[] args01 = { module, fun, argString(args) };
            ErlLogger.debug("cast -> %s:%s(%s)", args01);
        }
        send(node, peer, "rex", msg);
        if (CHECK_RPC) {
            ErlLogger.debug("RPC _cast_" + "=> " + msg);
        }
    }

    private OtpErlangObject[] convertArgs(final String signature, final Object... args)
            throws SignatureException {
        final Object[] args0 = args == null ? new OtpErlangObject[] {} : args;

        Signature[] type;
        type = Signature.parse(signature);
        if (type == null) {
            type = new Signature[args0.length];
            for (int i = 0; i < args0.length; i++) {
                type[i] = new Signature('x');
            }
        }
        if (type.length != args0.length) {
            throw new SignatureException("Signature doesn't match parameter number: "
                    + type.length + "/" + args0.length);
        }
        final OtpErlangObject[] args1 = new OtpErlangObject[args0.length];
        for (int i = 0; i < args1.length; i++) {
            args1[i] = TypeConverter.java2erlang(args0[i], type[i]);
        }
        return args1;
    }

    private OtpErlangObject buildRpcCastMsg(final OtpErlangObject gleader,
            final String module, final String fun, final OtpErlangObject[] args) {
        final OtpErlangObject m = new OtpErlangAtom(module);
        final OtpErlangObject f = new OtpErlangAtom(fun);
        final OtpErlangObject a = new OtpErlangList(args);
        final OtpErlangAtom castTag = new OtpErlangAtom("$gen_cast");
        return OtpErlang.mkTuple(castTag,
                OtpErlang.mkTuple(new OtpErlangAtom("cast"), m, f, a, gleader));
    }

}
