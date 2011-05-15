/*******************************************************************************
 * Copyright (c) 2010 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.backend.internal;

import java.io.IOException;

import org.erlide.core.backend.IErlRuntime;
import org.erlide.core.rpc.RpcCallback;
import org.erlide.core.rpc.RpcException;
import org.erlide.core.rpc.RpcFuture;
import org.erlide.core.rpc.RpcHelper;
import org.erlide.core.rpc.RpcResultCallback;
import org.erlide.jinterface.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.erlang.OtpNodeStatus;
import com.ericsson.otp.erlang.SignatureException;

public class ErlRuntime extends OtpNodeStatus implements IErlRuntime {
    private static final int MAX_RETRIES = 20;
    public static final int RETRY_DELAY = Integer.parseInt(System.getProperty(
            "erlide.connect.delay", "300"));
    private static final Object connectLock = new Object();

    public enum State {
        CONNECTED, DISCONNECTED, DOWN
    }

    private final String peerName;
    private State state;
    private OtpNode localNode;

    public ErlRuntime(final String name, final String cookie) {
        state = State.DISCONNECTED;
        try {
            localNode = ErlRuntime.createOtpNode(cookie);
            localNode.registerStatusHandler(this);
        } catch (final IOException e) {
            ErlLogger.error(e);
        }

        peerName = name;
        // if (epmdWatcher.isRunningNode(name)) {
        // connect();
        // }
    }

    public String getNodeName() {
        return peerName;
    }

    private boolean connectRetry() {
        int tries = MAX_RETRIES;
        boolean ok = false;
        while (!ok && tries > 0) {
            ErlLogger.debug("# ping..." + getNodeName() + " "
                    + Thread.currentThread().getName());
            ok = localNode.ping(getNodeName(), RETRY_DELAY
                    + (MAX_RETRIES - tries) * RETRY_DELAY % 3);
            tries--;
        }
        return ok;
    }

    public boolean connect() {
        return localNode.ping(getNodeName(), RETRY_DELAY);
    }

    @Override
    public void remoteStatus(final String node, final boolean up,
            final Object info) {
        if (node.equals(peerName)) {
            if (up) {
                ErlLogger.debug("Node %s is up", peerName);
                connectRetry();
            } else {
                ErlLogger.debug("Node %s is down: %s", peerName, info);
                state = State.DOWN;
            }
        }
    }

    public void makeAsyncResultCall(final RpcResultCallback cb, final String m,
            final String f, final String signature, final Object[] args)
            throws SignatureException {
        final OtpErlangAtom gleader = new OtpErlangAtom("user");
        RpcHelper.rpcCastWithProgress(cb, localNode, peerName, false, gleader,
                m, f, signature, args);
    }

    public RpcFuture makeAsyncCall(final OtpErlangObject gleader,
            final String module, final String fun, final String signature,
            final Object... args0) throws RpcException, SignatureException {
        tryConnect();
        return RpcHelper.sendRpcCall(localNode, peerName, false, gleader,
                module, fun, signature, args0);
    }

    public RpcFuture makeAsyncCall(final String module, final String fun,
            final String signature, final Object... args0) throws RpcException,
            SignatureException {
        return makeAsyncCall(new OtpErlangAtom("user"), module, fun, signature,
                args0);
    }

    public void makeAsyncCbCall(final RpcCallback cb, final int timeout,
            final String module, final String fun, final String signature,
            final Object... args) throws RpcException, SignatureException {
        makeAsyncCbCall(cb, timeout, new OtpErlangAtom("user"), module, fun,
                signature, args);
    }

    public void makeAsyncCbCall(final RpcCallback cb, final int timeout,
            final OtpErlangObject gleader, final String module,
            final String fun, final String signature, final Object... args)
            throws RpcException, SignatureException {
        tryConnect();
        RpcHelper.makeAsyncCbCall(localNode, peerName, cb, timeout, gleader,
                module, fun, signature, args);
    }

    public OtpErlangObject makeCall(final int timeout,
            final OtpErlangObject gleader, final String module,
            final String fun, final String signature, final Object... args0)
            throws RpcException, SignatureException {
        tryConnect();
        final OtpErlangObject result = RpcHelper.rpcCall(localNode, peerName,
                false, gleader, module, fun, timeout, signature, args0);
        return result;
    }

    public OtpErlangObject makeCall(final int timeout, final String module,
            final String fun, final String signature, final Object... args0)
            throws RpcException, SignatureException {
        return makeCall(timeout, new OtpErlangAtom("user"), module, fun,
                signature, args0);
    }

    public void makeCast(final OtpErlangObject gleader, final String module,
            final String fun, final String signature, final Object... args0)
            throws SignatureException, RpcException {
        tryConnect();
        RpcHelper.rpcCast(localNode, peerName, false, gleader, module, fun,
                signature, args0);
    }

    public void makeCast(final String module, final String fun,
            final String signature, final Object... args0)
            throws SignatureException, RpcException {
        makeCast(new OtpErlangAtom("user"), module, fun, signature, args0);
    }

    private void tryConnect() throws RpcException {
        synchronized (connectLock) {
            switch (state) {
            case DISCONNECTED:
                if (connectRetry()) {
                    state = State.CONNECTED;
                } else {
                    state = State.DISCONNECTED;
                }
                break;
            case CONNECTED:
                break;
            case DOWN:
                final String msg = "BackendImpl %s is down";
                throw new RpcException(String.format(msg, peerName));
            }
        }
    }

    public boolean isAvailable() {
        return state == State.CONNECTED;
    }

    public OtpNode getNode() {
        return localNode;
    }

    public static String createJavaNodeName() {
        final String fUniqueId = ErlRuntime.getTimeSuffix();
        return "jerlide_" + fUniqueId;
    }

    static String getTimeSuffix() {
        String fUniqueId;
        fUniqueId = Long.toHexString(System.currentTimeMillis() & 0xFFFFFFF);
        return fUniqueId;
    }

    public static OtpNode createOtpNode(final String cookie) throws IOException {
        OtpNode node;
        if (cookie == null) {
            node = new OtpNode(createJavaNodeName());
        } else {
            node = new OtpNode(createJavaNodeName(), cookie);
        }
        final String nodeCookie = node.cookie();
        final int len = nodeCookie.length();
        final String trimmed = len > 7 ? nodeCookie.substring(0, 7)
                : nodeCookie;
        ErlLogger.debug("using cookie '%s...'%d (info: '%s')", trimmed, len,
                cookie);
        return node;
    }

}
