package org.erlide.runtime.api;

import org.erlide.runtime.rpc.IRpcCallback;
import org.erlide.runtime.rpc.IRpcFuture;
import org.erlide.runtime.rpc.IRpcHelper;
import org.erlide.runtime.rpc.IRpcResultCallback;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.rpc.RpcResult;
import org.erlide.util.ExtensionUtils;
import org.erlide.util.erlang.SignatureException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpNode;

public class RpcSite implements IRpcSite {

    public static long DEFAULT_TIMEOUT;
    {
        setDefaultTimeout();
    }

    private final IErlRuntime runtime;
    private final String nodeName;
    private final OtpNode localNode;

    private static final IRpcHelper rpcHelper = ExtensionUtils
            .getSingletonExtension("org.erlide.runtime.api.rpc_helper",
                    IRpcHelper.class);

    public RpcSite(final IErlRuntime runtime, final OtpNode localNode,
            final String nodeName) {
        this.runtime = runtime;
        this.localNode = localNode;
        this.nodeName = nodeName;
    }

    @Override
    public void async_call_result(final IRpcResultCallback cb, final String m,
            final String f, final String signature, final Object... args)
            throws RpcException {
        final OtpErlangAtom gleader = new OtpErlangAtom("user");
        try {
            rpcHelper.rpcCastWithProgress(cb, localNode, nodeName, false,
                    gleader, m, f, signature, args);
        } catch (final SignatureException e) {
            throw new RpcException(e);
        }
    }

    @Override
    public IRpcFuture async_call(final OtpErlangObject gleader,
            final String module, final String fun, final String signature,
            final Object... args0) throws RpcException {
        runtime.tryConnect();
        try {
            return rpcHelper.sendRpcCall(localNode, nodeName, false, gleader,
                    module, fun, signature, args0);
        } catch (final SignatureException e) {
            throw new RpcException(e);
        }
    }

    @Override
    public IRpcFuture async_call(final String module, final String fun,
            final String signature, final Object... args0) throws RpcException {
        return async_call(new OtpErlangAtom("user"), module, fun, signature,
                args0);
    }

    @Override
    public void async_call_cb(final IRpcCallback cb, final long timeout,
            final String module, final String fun, final String signature,
            final Object... args) throws RpcException {
        async_call_cb(cb, timeout, new OtpErlangAtom("user"), module, fun,
                signature, args);
    }

    @Override
    public void async_call_cb(final IRpcCallback cb, final long timeout,
            final OtpErlangObject gleader, final String module,
            final String fun, final String signature, final Object... args)
            throws RpcException {
        runtime.tryConnect();
        try {
            rpcHelper.makeAsyncCbCall(localNode, nodeName, cb, timeout,
                    gleader, module, fun, signature, args);
        } catch (final SignatureException e) {
            throw new RpcException(e);
        }
    }

    @Override
    public OtpErlangObject call(final long timeout,
            final OtpErlangObject gleader, final String module,
            final String fun, final String signature, final Object... args0)
            throws RpcException {
        runtime.tryConnect();
        OtpErlangObject result;
        try {
            result = rpcHelper.rpcCall(localNode, nodeName, false, gleader,
                    module, fun, timeout, signature, args0);
        } catch (final SignatureException e) {
            throw new RpcException(e);
        }
        return result;
    }

    @Override
    public OtpErlangObject call(final long timeout, final String module,
            final String fun, final String signature, final Object... args0)
            throws RpcException {
        return call(timeout, new OtpErlangAtom("user"), module, fun, signature,
                args0);
    }

    @Override
    public void cast(final OtpErlangObject gleader, final String module,
            final String fun, final String signature, final Object... args0)
            throws RpcException {
        runtime.tryConnect();
        try {
            rpcHelper.rpcCast(localNode, nodeName, false, gleader, module, fun,
                    signature, args0);
        } catch (final SignatureException e) {
            throw new RpcException(e);
        }
    }

    @Override
    public void cast(final String module, final String fun,
            final String signature, final Object... args0) throws RpcException {
        cast(new OtpErlangAtom("user"), module, fun, signature, args0);
    }

    @Override
    public void send(final OtpErlangPid pid, final Object msg) {
        try {
            runtime.tryConnect();
            rpcHelper.send(localNode, pid, msg);
        } catch (final Exception e) {
        }
    }

    @Override
    public void send(final String fullNodeName, final String name,
            final Object msg) {
        try {
            runtime.tryConnect();
            rpcHelper.send(localNode, fullNodeName, name, msg);
        } catch (final Exception e) {
        }
    }

    @Override
    public RpcResult call_noexception(final String m, final String f,
            final String signature, final Object... args) {
        return call_noexception(DEFAULT_TIMEOUT, m, f, signature, args);
    }

    @Override
    public RpcResult call_noexception(final long timeout, final String m,
            final String f, final String signature, final Object... args) {
        try {
            final OtpErlangObject result = call(timeout, m, f, signature, args);
            return new RpcResult(result);
        } catch (final RpcException e) {
            return RpcResult.error(e.getMessage());
        }
    }

    @Override
    public void async_call_cb(final IRpcCallback cb, final String m,
            final String f, final String signature, final Object... args)
            throws RpcException {
        async_call_cb(cb, DEFAULT_TIMEOUT, m, f, signature, args);
    }

    @Override
    public OtpErlangObject call(final String m, final String f,
            final String signature, final Object... a) throws RpcException {
        return call(DEFAULT_TIMEOUT, m, f, signature, a);
    }

    @Override
    public void send(final String name, final Object msg) {
        try {
            runtime.tryConnect();
            rpcHelper.send(localNode, nodeName, name, msg);
        } catch (final Exception e) {
        }
    }

    private static void setDefaultTimeout() {
        final String t = System.getProperty("erlide.rpc.timeout", "9000");
        if ("infinity".equals(t)) {
            DEFAULT_TIMEOUT = IRpcHelper.INFINITY;
        } else {
            try {
                DEFAULT_TIMEOUT = Integer.parseInt(t);
            } catch (final Exception e) {
                DEFAULT_TIMEOUT = 9000;
            }
        }
    }

}
