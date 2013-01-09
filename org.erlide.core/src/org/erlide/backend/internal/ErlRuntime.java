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
package org.erlide.backend.internal;

import java.io.IOException;
import java.net.Socket;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.IProcess;
import org.erlide.backend.BackendException;
import org.erlide.core.MessageReporter;
import org.erlide.core.MessageReporter.ReporterPosition;
import org.erlide.runtime.HostnameUtils;
import org.erlide.runtime.IErlRuntime;
import org.erlide.runtime.IRpcSite;
import org.erlide.runtime.RuntimeData;
import org.erlide.runtime.rpc.IRpcCallback;
import org.erlide.runtime.rpc.IRpcFuture;
import org.erlide.runtime.rpc.IRpcResultCallback;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.rpc.RpcHelper;
import org.erlide.runtime.rpc.RpcResult;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.utils.ErlLogger;
import org.erlide.utils.IProvider;
import org.erlide.utils.SystemConfiguration;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.erlang.OtpNodeStatus;
import com.ericsson.otp.erlang.SignatureException;
import com.google.common.base.Strings;

public class ErlRuntime implements IErlRuntime, IRpcSite {
    private static final String COULD_NOT_CONNECT_TO_BACKEND = "Could not connect to backend! Please check runtime settings.";
    private static final int EPMD_PORT = 4369;
    public static int DEFAULT_TIMEOUT;
    {
        setDefaultTimeout();
    }

    private static final int MAX_RETRIES = 10;
    public static final int RETRY_DELAY = Integer.parseInt(System.getProperty(
            "erlide.connect.delay", "300"));
    private static final Object connectLock = new Object();
    private static final RpcHelper rpcHelper = RpcHelper.getInstance();

    public enum State {
        CONNECTED, DISCONNECTED, DOWN
    }

    private State state;
    private final RuntimeData data;
    private OtpNode localNode;
    private final Object localNodeLock = new Object();
    private boolean reported;
    private final IProcess process;
    private final boolean connectOnce;
    private final IProvider<IProcess> processProvider;
    private final OtpNodeStatus statusWatcher;
    private OtpMbox eventBox;
    private boolean stopped;

    public ErlRuntime(final String name, final String cookie,
            final IProvider<IProcess> processProvider,
            final boolean reportWhenDown, final boolean longName,
            final boolean connectOnce) {
        state = State.DISCONNECTED;
        this.processProvider = processProvider;
        process = processProvider.get();
        this.connectOnce = connectOnce;
        stopped = false;
        data = new RuntimeData();
        data.setLongName(longName);
        data.setCookie(cookie);
        data.setNodeName(name);
        data.setReportErrors(reportWhenDown);

        statusWatcher = new OtpNodeStatus() {
            @Override
            public void remoteStatus(final String node, final boolean up,
                    final Object info) {
                if (node.equals(data.getNodeName())) {
                    if (up) {
                        ErlLogger.debug("Node %s is up", data.getNodeName());
                        connectRetry();
                    } else {
                        ErlLogger.debug("Node %s is down: %s",
                                data.getNodeName(), info);
                        state = State.DOWN;
                    }
                }
            }
        };
        startLocalNode();
        // if (epmdWatcher.isRunningNode(name)) {
        // connect();
        // }
    }

    @Override
    public void start() {
        // TODO Auto-generated method stub

    }

    public void startLocalNode() {
        boolean nodeCreated = false;
        synchronized (localNodeLock) {
            int i = 0;
            do {
                try {
                    i++;
                    localNode = ErlRuntime.createOtpNode(data.getCookie(),
                            data.hasLongName());
                    localNode.registerStatusHandler(statusWatcher);
                    nodeCreated = true;
                } catch (final IOException e) {
                    ErlLogger
                            .error("ErlRuntime could not be created (%s), retrying %d",
                                    e.getMessage(), i);
                    try {
                        localNodeLock.wait(300);
                    } catch (final InterruptedException e1) {
                    }
                }
            } while (!nodeCreated && i < 10);

        }
    }

    @Override
    public String getNodeName() {
        return data.getNodeName();
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

    @Override
    public void async_call_result(final IRpcResultCallback cb, final String m,
            final String f, final String signature, final Object... args)
            throws RpcException {
        final OtpErlangAtom gleader = new OtpErlangAtom("user");
        try {
            rpcHelper.rpcCastWithProgress(cb, localNode, data.getNodeName(),
                    false, gleader, m, f, signature, args);
        } catch (final SignatureException e) {
            throw new RpcException(e);
        }
    }

    @Override
    public IRpcFuture async_call(final OtpErlangObject gleader,
            final String module, final String fun, final String signature,
            final Object... args0) throws RpcException {
        tryConnect();
        try {
            return rpcHelper.sendRpcCall(localNode, data.getNodeName(), false,
                    gleader, module, fun, signature, args0);
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
    public void async_call_cb(final IRpcCallback cb, final int timeout,
            final String module, final String fun, final String signature,
            final Object... args) throws RpcException {
        async_call_cb(cb, timeout, new OtpErlangAtom("user"), module, fun,
                signature, args);
    }

    @Override
    public void async_call_cb(final IRpcCallback cb, final int timeout,
            final OtpErlangObject gleader, final String module,
            final String fun, final String signature, final Object... args)
            throws RpcException {
        tryConnect();
        try {
            rpcHelper.makeAsyncCbCall(localNode, data.getNodeName(), cb,
                    timeout, gleader, module, fun, signature, args);
        } catch (final SignatureException e) {
            throw new RpcException(e);
        }
    }

    @Override
    public OtpErlangObject call(final int timeout,
            final OtpErlangObject gleader, final String module,
            final String fun, final String signature, final Object... args0)
            throws RpcException {
        tryConnect();
        OtpErlangObject result;
        try {
            result = rpcHelper.rpcCall(localNode, data.getNodeName(), false,
                    gleader, module, fun, timeout, signature, args0);
        } catch (final SignatureException e) {
            throw new RpcException(e);
        }
        return result;
    }

    @Override
    public OtpErlangObject call(final int timeout, final String module,
            final String fun, final String signature, final Object... args0)
            throws RpcException {
        return call(timeout, new OtpErlangAtom("user"), module, fun, signature,
                args0);
    }

    @Override
    public void cast(final OtpErlangObject gleader, final String module,
            final String fun, final String signature, final Object... args0)
            throws RpcException {
        tryConnect();
        try {
            rpcHelper.rpcCast(localNode, data.getNodeName(), false, gleader,
                    module, fun, signature, args0);
        } catch (final SignatureException e) {
            throw new RpcException(e);
        }
    }

    @Override
    public void cast(final String module, final String fun,
            final String signature, final Object... args0) throws RpcException {
        cast(new OtpErlangAtom("user"), module, fun, signature, args0);
    }

    private void tryConnect() throws RpcException {
        synchronized (connectLock) {
            switch (state) {
            case DISCONNECTED:
                reported = false;
                if (connectRetry()) {
                    state = State.CONNECTED;
                } else if (connectOnce) {
                    state = State.DOWN;
                } else {
                    state = State.DISCONNECTED;
                }
                break;
            case CONNECTED:
                break;
            case DOWN:
                final String msg = reportRuntimeDown(data.getNodeName());
                try {
                    if (process != null) {
                        process.terminate();
                    }
                } catch (final DebugException e) {
                    ErlLogger.info(e);
                }
                // TODO restart it??
                // process =
                throw new RpcException(msg);
            }
        }
    }

    private String reportRuntimeDown(final String peer) {
        final String fmt = "Backend '%s' is down";
        final String msg = String.format(fmt, peer);
        if (data.getReportErrors() && !reported) {
            final String user = System.getProperty("user.name");

            String msg1;
            if (connectOnce) {
                msg1 = "It is likely that your network is misconfigured or uses 'strange' host names.\n"
                        + "Please check the "
                        + "Window->preferences->erlang->network page for hints about that."
                        + "\n\n"
                        + "Also, check if you can create and connect two erlang nodes on your machine\n"
                        + "using \"erl -name foo1\" and \"erl -name foo2\".";
            } else {
                msg1 = "If you didn't shut it down on purpose, it is an "
                        + "unrecoverable error, please restart Eclipse. ";
            }

            final String bigMsg = msg
                    + "\n\n"
                    + msg1
                    + "\n\n"
                    + "If an error report named '"
                    + user
                    + "_<timestamp>.txt' has been created in your home directory,\n "
                    + "please consider reporting the problem. \n"
                    + (SystemConfiguration
                            .hasFeatureEnabled("erlide.ericsson.user") ? ""
                            : "http://www.assembla.com/spaces/erlide/support/tickets");
            MessageReporter.showError(bigMsg, ReporterPosition.CORNER);
            reported = true;
        }
        return msg;
    }

    @Override
    public boolean isAvailable() {
        return state == State.CONNECTED;
    }

    public static String createJavaNodeName() {
        final String fUniqueId = ErlRuntime.getTimeSuffix();
        return "jerlide_" + fUniqueId;
    }

    public static String createJavaNodeName(final String hostName) {
        return createJavaNodeName() + "@" + hostName;
    }

    static String getTimeSuffix() {
        String fUniqueId;
        fUniqueId = Long.toHexString(System.currentTimeMillis() & 0xFFFFFFF);
        return fUniqueId;
    }

    public static OtpNode createOtpNode(final String cookie,
            final boolean longName) throws IOException {
        OtpNode node;
        final String hostName = HostnameUtils.getErlangHostName(longName);
        if (Strings.isNullOrEmpty(cookie)) {
            node = new OtpNode(createJavaNodeName(hostName));
        } else {
            node = new OtpNode(createJavaNodeName(hostName), cookie);
        }
        debugPrintCookie(node.cookie());
        return node;
    }

    private static void debugPrintCookie(final String cookie) {
        final int len = cookie.length();
        final String trimmed = len > 7 ? cookie.substring(0, 7) : cookie;
        ErlLogger.debug("using cookie '%s...'%d (info: '%s')", trimmed, len,
                cookie);
    }

    @Override
    public void send(final OtpErlangPid pid, final Object msg) {
        try {
            tryConnect();
            rpcHelper.send(localNode, pid, msg);
        } catch (final SignatureException e) {
        } catch (final RpcException e) {
        }
    }

    @Override
    public void send(final String fullNodeName, final String name,
            final Object msg) {
        try {
            tryConnect();
            rpcHelper.send(localNode, fullNodeName, name, msg);
        } catch (final SignatureException e) {
        } catch (final RpcException e) {
        }
    }

    @Override
    public OtpMbox createMbox(final String name) {
        return localNode.createMbox(name);
    }

    @Override
    public OtpMbox createMbox() {
        return localNode.createMbox();
    }

    @Override
    public void stop() {
        // close peer too?
        stopped = true;
        localNode.close();
    }

    @Override
    public RpcResult call_noexception(final String m, final String f,
            final String signature, final Object... args) {
        return call_noexception(DEFAULT_TIMEOUT, m, f, signature, args);
    }

    @Override
    public RpcResult call_noexception(final int timeout, final String m,
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
            tryConnect();
            rpcHelper.send(localNode, data.getNodeName(), name, msg);
        } catch (final SignatureException e) {
        } catch (final RpcException e) {
        }
    }

    @Override
    public void connect() {
        final String label = getNodeName();
        ErlLogger.debug(label + ": waiting connection to peer...");
        try {
            wait_for_epmd();
            eventBox = createMbox("rex");

            if (waitForCodeServer()) {
                ErlLogger.debug("connected!");
            } else {
                ErlLogger.error(COULD_NOT_CONNECT_TO_BACKEND);
            }

        } catch (final BackendException e) {
            ErlLogger.error(e);
            ErlLogger.error(COULD_NOT_CONNECT_TO_BACKEND);
        } catch (final Exception e) {
            ErlLogger.error(e);
            ErlLogger.error(COULD_NOT_CONNECT_TO_BACKEND);
        }
    }

    private OtpMbox getEventBox() {
        return eventBox;
    }

    @Override
    public OtpErlangPid getEventPid() {
        final OtpMbox theEventBox = getEventBox();
        if (theEventBox == null) {
            return null;
        }
        return theEventBox.self();
    }

    private void wait_for_epmd() throws BackendException {
        wait_for_epmd("localhost");
    }

    private void wait_for_epmd(final String host) throws BackendException {
        // If anyone has a better solution for waiting for epmd to be up, please
        // let me know
        int tries = 50;
        boolean ok = false;
        do {
            Socket s;
            try {
                s = new Socket(host, EPMD_PORT);
                s.close();
                ok = true;
            } catch (final IOException e) {
            }
            try {
                Thread.sleep(100);
                // ErlLogger.debug("sleep............");
            } catch (final InterruptedException e1) {
            }
            tries--;
        } while (!ok && tries > 0);
        if (!ok) {
            final String msg = "Couldn't contact epmd - erlang backend is probably not working\n"
                    + "  Possibly your host's entry in /etc/hosts is wrong ("
                    + host + ").";
            ErlLogger.error(msg);
            throw new BackendException(msg);
        }
    }

    private boolean waitForCodeServer() {
        try {
            OtpErlangObject r;
            int i = 30;
            boolean gotIt = false;
            do {
                r = call("erlang", "whereis", "a", "code_server");
                gotIt = !(r instanceof OtpErlangPid);
                if (!gotIt) {
                    try {
                        Thread.sleep(200);
                    } catch (final InterruptedException e) {
                    }
                }
                i--;
            } while (gotIt && i > 0);
            if (gotIt) {
                ErlLogger.error("code server did not start in time for %s",
                        getNodeName());
                return false;
            }
            ErlLogger.debug("code server started");
            return true;
        } catch (final Exception e) {
            ErlLogger.error("error starting code server for %s: %s",
                    getNodeName(), e.getMessage());
            return false;
        }
    }

    private static void setDefaultTimeout() {
        final String t = System.getProperty("erlide.rpc.timeout", "9000");
        if ("infinity".equals(t)) {
            DEFAULT_TIMEOUT = RpcHelper.INFINITY;
        } else {
            try {
                DEFAULT_TIMEOUT = Integer.parseInt(t);
            } catch (final Exception e) {
                DEFAULT_TIMEOUT = 9000;
            }
        }
    }

    @Override
    public boolean isStopped() {
        return stopped || !isAvailable();
    }

    @Override
    public OtpMbox getEventMbox() {
        return eventBox;
    }

    @Override
    public IRpcSite getRpcSite() {
        return this;
    }

    @Override
    public RuntimeData getRuntimeData() {
        return data;
    }

    @Override
    public RuntimeInfo getRuntimeInfo() {
        return data.getRuntimeInfo();
    }
}
