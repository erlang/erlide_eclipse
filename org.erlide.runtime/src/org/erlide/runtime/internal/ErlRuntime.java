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
package org.erlide.runtime.internal;

import java.io.File;
import java.io.IOException;
import java.net.Socket;
import java.util.Arrays;
import java.util.Map;

import org.erlide.runtime.api.ErlSystemStatus;
import org.erlide.runtime.api.IErlRuntime;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.IRuntimeStateListener;
import org.erlide.runtime.api.RuntimeData;
import org.erlide.runtime.events.ErlangEventPublisher;
import org.erlide.runtime.events.ErlangLogEventHandler;
import org.erlide.runtime.events.LogEventHandler;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.rpc.RpcSite;
import org.erlide.runtime.shell.IBackendShell;
import org.erlide.util.ErlLogger;
import org.erlide.util.HostnameUtils;
import org.erlide.util.SystemConfiguration;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.erlang.OtpNodeStatus;
import com.google.common.base.Strings;

public class ErlRuntime implements IErlRuntime {
    private static final String COULD_NOT_CONNECT = "Could not connect to backend! Please check runtime settings.";
    private static final int EPMD_PORT = Integer.parseInt(System.getProperty(
            "erlide.epmd.port", "4369"));

    private static final int MAX_RETRIES = 15;
    public static final int RETRY_DELAY = Integer.parseInt(System.getProperty(
            "erlide.connect.delay", "400"));

    public enum State {
        DISCONNECTED, CONNECTED, DOWN
    }

    private volatile State state;
    private final RuntimeData data;
    private OtpNode localNode;
    private final Object localNodeLock = new Object();
    private boolean reported;
    private Process process;
    private OtpMbox eventMBox;
    private boolean stopped;
    private IRuntimeStateListener listener;
    private ErlSystemStatus lastSystemMessage;
    private final IRpcSite rpcSite;
    protected ErlangEventPublisher eventDaemon;

    public ErlRuntime(final RuntimeData data) {
        this.data = data;
        final String nodeName = getNodeName();
        start();
        rpcSite = new RpcSite(this, localNode, nodeName);

        eventDaemon = new ErlangEventPublisher(this);
        eventDaemon.start();
        eventDaemon.register(new LogEventHandler(nodeName));
        eventDaemon.register(new ErlangLogEventHandler(nodeName));
    }

    @Override
    public Process start() {
        startLocalNode();
        state = State.DISCONNECTED;
        stopped = false;

        process = startRuntimeProcess(data);
        return process;
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
                    final OtpNodeStatus statusWatcher = new ErlideNodeStatus();
                    localNode.registerStatusHandler(statusWatcher);
                    eventMBox = createMbox("rex");
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
        return data.getQualifiedNodeName();
    }

    public boolean connectRetry() {
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

    private void handleStateDisconnected() {
        reported = false;
        if (connectRetry()) {
            state = State.CONNECTED;
            rpcSite.setConnected(true);

            if (waitForCodeServer()) {
                ErlLogger.debug("connected!");
            } else {
                ErlLogger.error(COULD_NOT_CONNECT);
            }

        } else {
            state = State.DOWN;
            rpcSite.setConnected(false);
        }
    }

    private void handleStateDown() throws RpcException {
        if (listener != null) {
            listener.runtimeDown(this);
        }
        rpcSite.setConnected(false);
        if (process != null) {
            process.destroy();
            process = null;
        }
        if (!stopped) {
            final String msg = reportRuntimeDown(getNodeName());
            // throw new RpcException(msg);
        }
    }

    private String reportRuntimeDown(final String peer) {
        final String fmt = "Backend '%s' is down";
        final String msg = String.format(fmt, peer);
        // TODO when to report errors?
        final boolean shouldReport = data.isInternal() || data.isReportErrors();
        if (shouldReport && !reported) {
            final String user = System.getProperty("user.name");

            String msg1;
            if (data.isInternal()) {
                msg1 = "It is likely that your network is misconfigured or uses 'strange' host names.\n\n"
                        + "Please check the page"
                        + "Window->preferences->erlang->network for hints about that. \n\n"
                        + "Also, check if you can create and connect two erlang nodes on your machine "
                        + "using \"erl -name foo1\" and \"erl -name foo2\".";
            } else {
                msg1 = "If you didn't shut it down on purpose, it is an "
                        + "unrecoverable error, please restart Eclipse. ";
            }

            final String details = "If an error report named '"
                    + user
                    + "_<timestamp>.txt' has been created in your home directory, "
                    + "please consider reporting the problem. \n"
                    + (SystemConfiguration
                            .hasFeatureEnabled("erlide.ericsson.user") ? ""
                            : "http://www.assembla.com/spaces/erlide/support/tickets");
            // FIXME MessageReporter.showError(msg, msg1 + "\n\n" + details);
            reported = true;
        }

        final ErlSystemStatus status = getSystemStatus();
        ErlLogger.error("Last system status was:\n %s",
                status != null ? status.prettyPrint() : "null");

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
        if (stopped) {
            return;
        }
        stopped = true;
        localNode.close();
    }

    @Override
    public void connect() {
        final String label = getNodeName();
        ErlLogger.debug(label + ": waiting connection to peer...");
        try {
            wait_for_epmd();
            System.out.println("wait for it");
            connectRetry();
            int i = 0;
            while (state == State.DISCONNECTED && i++ < 10) {
                System.out.println("##### " + state);
                try {
                    Thread.sleep(200);
                } catch (final InterruptedException e) {
                }
            }
        } catch (final Exception e) {
            ErlLogger.error(e);
            ErlLogger.error(COULD_NOT_CONNECT);
        }
    }

    private void wait_for_epmd() throws Exception {
        wait_for_epmd(null);
    }

    private void wait_for_epmd(final String host) throws Exception {
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
                // ErlLogger.debug("sleep............");
                Thread.sleep(100);
            } catch (final InterruptedException e1) {
            }
            tries--;
        } while (!ok && tries > 0);
        if (!ok) {
            final String msg = "Couldn't contact epmd - erlang backend is probably not working\n"
                    + "Your host's entry in /etc/hosts is probably wrong ("
                    + host + ").";
            ErlLogger.error(msg);
            throw new Exception(msg);
        }
    }

    private boolean waitForCodeServer() {
        try {
            OtpErlangObject r;
            int i = 30;
            boolean gotIt = false;
            do {
                r = rpcSite.call("erlang", "whereis", "a", "code_server");
                System.out.println(r);
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

    @Override
    public boolean isRunning() {
        return !stopped;
    }

    @Override
    public OtpMbox getEventMbox() {
        return eventMBox;
    }

    @Override
    public RuntimeData getRuntimeData() {
        return data;
    }

    @Override
    public void addListener(final IRuntimeStateListener aListener) {
        listener = aListener;
    }

    @Override
    public IBackendShell getShell(final String id) {
        // TODO can we return something here?
        return null;
    }

    @Override
    public ErlSystemStatus getSystemStatus() {
        return lastSystemMessage;
    }

    @Override
    public void setSystemStatus(final ErlSystemStatus msg) {
        // System.out.println(msg.prettyPrint());
        lastSystemMessage = msg;
    }

    @Override
    public void dispose() {
        stop();
        if (eventDaemon != null) {
            eventDaemon.stop();
            eventDaemon = null;
        }
        process = null;
        listener = null;
    }

    @Override
    public IRpcSite getRpcSite() {
        return rpcSite;
    }

    private Process startRuntimeProcess(final RuntimeData rtData) {
        final String[] cmds = rtData.getCmdLine();
        final File workingDirectory = new File(rtData.getWorkingDir());

        try {
            ErlLogger.debug("START node :> " + Arrays.toString(cmds) + " *** "
                    + workingDirectory.getCanonicalPath());
        } catch (final IOException e1) {
            ErlLogger.error("START node :> " + e1.getMessage());
        }

        final ProcessBuilder builder = new ProcessBuilder(cmds);
        builder.directory(workingDirectory);
        setEnvironment(rtData, builder);
        try {
            Process aProcess = builder.start();
            try {
                final int code = aProcess.exitValue();
                ErlLogger.error(
                        "Could not create runtime (exit code = %d): %s", code,
                        Arrays.toString(cmds));
                aProcess = null;
            } catch (final IllegalThreadStateException e) {
                ErlLogger.debug("process is running");
            }
            return aProcess;
        } catch (final IOException e) {
            ErlLogger.error("Could not create runtime: %s",
                    Arrays.toString(cmds));
            ErlLogger.error(e);
            return null;
        }
    }

    private void setEnvironment(final RuntimeData data,
            final ProcessBuilder builder) {
        final Map<String, String> env = builder.environment();
        if (!SystemConfiguration.getInstance().isOnWindows()
                && SystemConfiguration.getInstance().hasSpecialTclLib()) {
            env.put("TCL_LIBRARY", "/usr/share/tcl/tcl8.4/");
        }
        if (data.getEnv() != null) {
            env.putAll(data.getEnv());
        }
    }

    @Override
    public Process getProcess() {
        return process;
    }

    @Override
    public void registerEventHandler(final Object handler) {
        eventDaemon.register(handler);
    }

    private class ErlideNodeStatus extends OtpNodeStatus {
        @Override
        public void remoteStatus(final String node, final boolean up,
                final Object info) {

            ErlLogger.debug("@@@remote " + node + " " + (up ? "up" : "down")
                    + " " + info);

            if (!node.equals(getNodeName())) {
                return;
            }
            if (up) {
                ErlLogger.debug("Node %s is up", getNodeName());
                handleStateDisconnected();
            } else {
                ErlLogger.debug("Node %s is down: %s", getNodeName(), info);
                state = State.DOWN;
                try {
                    handleStateDown();
                } catch (final RpcException e) {
                    e.printStackTrace();
                }
            }
        }

        @Override
        public void localStatus(final String node, final boolean up,
                final Object info) {
            ErlLogger.debug("@@@local " + node + " " + (up ? "up" : "down")
                    + " " + info);
        }

        @Override
        public void connAttempt(final String node, final boolean incoming,
                final Object info) {
            ErlLogger.debug("@@@conn " + node + " " + (incoming ? "in" : "out")
                    + " " + info);
        }
    }
}
