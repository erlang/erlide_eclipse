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
import org.erlide.runtime.events.ErlEvent;
import org.erlide.runtime.events.ErlangLogEventHandler;
import org.erlide.runtime.events.LogEventHandler;
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
import com.google.common.eventbus.DeadEvent;
import com.google.common.eventbus.EventBus;
import com.google.common.eventbus.Subscribe;
import com.google.common.util.concurrent.AbstractExecutionThreadService;

public class ErlRuntime extends AbstractExecutionThreadService implements
        IErlRuntime {
    private static final String COULD_NOT_CONNECT = "Could not connect to backend! Please check runtime settings.";
    private static final int EPMD_PORT = Integer.parseInt(System.getProperty(
            "erlide.epmd.port", "4369"));

    private static final int MAX_RETRIES = 15;
    public static final int RETRY_DELAY = Integer.parseInt(System.getProperty(
            "erlide.connect.delay", "400"));

    private final RuntimeData data;
    private OtpNode localNode;
    private final Object localNodeLock = new Object();
    private final ErlRuntimeReporter reporter;
    private Process process;
    private OtpMbox eventMBox;
    private IRuntimeStateListener listener;
    private ErlSystemStatus lastSystemMessage;
    private IRpcSite rpcSite;
    private final EventBus eventBus;
    private volatile boolean stopped;
    public volatile int exitCode;
    private EventHelper eventHelper;

    final static boolean DEBUG = Boolean.parseBoolean(System
            .getProperty("erlide.event.daemon"));
    public static final long POLL_INTERVAL = 200;

    public ErlRuntime(final RuntimeData data) {
        this.data = data;
        reporter = new ErlRuntimeReporter(data.isInternal());

        final String nodeName = getNodeName();
        eventBus = new EventBus(nodeName);
        eventBus.register(this);
        registerEventListener(new LogEventHandler(nodeName));
        registerEventListener(new ErlangLogEventHandler(nodeName));
    }

    private void startLocalNode() throws IOException {
        synchronized (localNodeLock) {
            localNode = ErlRuntime.createOtpNode(data.getCookie(),
                    data.hasLongName());
            final OtpNodeStatus statusWatcher = new ErlideNodeStatus();
            localNode.registerStatusHandler(statusWatcher);
            eventMBox = createMbox("rex");
        }
    }

    @Override
    public String getNodeName() {
        return data.getQualifiedNodeName();
    }

    private boolean pingPeer() {
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

    private void connect() {
        final String label = getNodeName();
        ErlLogger.debug(label + ": waiting connection to peer...");
        try {
            wait_for_epmd();
            pingPeer();
            int i = 0;
            while (state() == State.NEW && i++ < 20) {
                System.out.println("STATE=" + state());
                try {
                    Thread.sleep(POLL_INTERVAL);
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
                gotIt = !(r instanceof OtpErlangPid);
                if (!gotIt) {
                    try {
                        Thread.sleep(POLL_INTERVAL);
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
        triggerShutdown();
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

    private class ErlideNodeStatus extends OtpNodeStatus {
        @Override
        public void remoteStatus(final String node, final boolean up,
                final Object info) {
            if (!node.equals(getNodeName())) {
                return;
            }
            if (!up) {
                ErlLogger.debug("Node %s is down: %s", getNodeName(), info);
                try {
                    exitCode = process.exitValue();
                } catch (final IllegalThreadStateException e) {
                    exitCode = -1;
                }
                triggerShutdown();
            }
        }
    }

    @Override
    protected void startUp() throws Exception {
        startLocalNode();
        exitCode = -1;
        process = startRuntimeProcess(data);
        rpcSite = new RpcSite(this, localNode, getNodeName());

        connect();
        ErlLogger.debug("Node %s is up", getNodeName());
        rpcSite.setConnected(true);

        if (waitForCodeServer()) {
            ErlLogger.debug("connected!");
        } else {
            triggerShutdown();
            ErlLogger.error(COULD_NOT_CONNECT);
        }

    }

    @Override
    protected void shutDown() throws Exception {
        localNode.close();

        process.destroy();
        process = null;

        if (listener != null) {
            listener.runtimeDown(ErlRuntime.this);
        }
        listener = null;

        rpcSite.setConnected(false);
        if (isRunning() && data.isReportErrors()) {
            final String msg = reporter.reportRuntimeDown(getNodeName(),
                    getSystemStatus());
            ErlLogger.error(msg);
        }
    }

    @Override
    protected void triggerShutdown() {
        stopped = true;
    }

    @Override
    protected void run() throws Exception {
        OtpErlangObject msg = null;
        eventHelper = new EventHelper();
        do {
            final OtpMbox eventBox = getEventMbox();
            msg = eventBox != null ? eventBox.receive(POLL_INTERVAL) : null;
            String topic = null;
            OtpErlangObject event = null;
            OtpErlangPid sender = null;
            if (msg != null) {
                if (!eventHelper.isEventMessage(msg)) {
                    // throw new Exception("Bad event data " + msg);
                    continue;
                }
                topic = eventHelper.getEventTopic(msg);
                event = eventHelper.getEventData(msg);
                sender = eventHelper.getEventSender(msg);
            }
            if (topic != null) {
                if (DEBUG) {
                    ErlLogger.debug("MSG: %s", "[" + sender + "::" + topic
                            + ": " + event + "]");
                }
                publishEvent(this, topic, event, sender);
            }
            if (exitCode > 0) {
                throw new RuntimeException("runtime crashed with code "
                        + exitCode);
            }
        } while (!stopped && exitCode < 0);
    }

    private void publishEvent(final IErlRuntime b, final String topic,
            final OtpErlangObject event, final OtpErlangPid sender) {
        final ErlEvent busEvent = new ErlEvent(topic, b, event, sender);
        eventBus.post(busEvent);
    }

    @Override
    public void registerEventListener(final Object handler) {
        eventBus.register(handler);
    }

    @Subscribe
    public void deadEventHandler(final DeadEvent dead) {
        ErlLogger.warn("Dead event: " + dead + " in " + getNodeName());
    }

    @Override
    protected String serviceName() {
        return getClass().getSimpleName() + " " + getNodeName();
    }
}
