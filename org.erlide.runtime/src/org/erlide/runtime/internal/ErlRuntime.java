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

import java.io.IOException;
import java.net.Socket;

import org.erlide.runtime.api.ErlSystemStatus;
import org.erlide.runtime.api.IErlRuntime;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.IShutdownCallback;
import org.erlide.runtime.api.RuntimeData;
import org.erlide.runtime.events.ErlEvent;
import org.erlide.runtime.events.ErlangLogEventHandler;
import org.erlide.runtime.events.LogEventHandler;
import org.erlide.runtime.internal.rpc.RpcSite;
import org.erlide.util.ErlLogger;
import org.erlide.util.HostnameUtils;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
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
    private static final String COULD_NOT_CONNECT = "Could not connect to %s! Please check runtime settings.";
    private static final int EPMD_PORT = Integer.parseInt(System.getProperty(
            "erlide.epmd.port", "4369"));

    private static final int MAX_RETRIES = 15;
    public static final int RETRY_DELAY = Integer.parseInt(System.getProperty(
            "erlide.connect.delay", "400"));

    protected final RuntimeData data;
    private OtpNode localNode;
    final ErlRuntimeReporter reporter;
    private OtpMbox eventMBox;
    private IShutdownCallback callback;
    private ErlSystemStatus lastSystemMessage;
    private IRpcSite rpcSite;
    private final EventBus eventBus;
    protected volatile boolean stopped;
    private EventParser eventHelper;
    boolean crashed;

    static final boolean DEBUG = Boolean.parseBoolean(System
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

        addListener(new ErlRuntimeListener(), executor());
    }

    @Override
    protected void startUp() throws Exception {
        localNode = startLocalNode();
        eventMBox = createMbox("rex");
        rpcSite = new RpcSite(this, localNode, getNodeName());
        connect();
        rpcSite.setConnected(true);

        if (!waitForCodeServer()) {
            triggerShutdown();
            ErlLogger.error(COULD_NOT_CONNECT, getNodeName());
        }
        stopped = false;
        crashed = false;
    }

    @Override
    protected void shutDown() throws Exception {
        localNode.close();

        if (callback != null) {
            callback.onShutdown();
        }
        callback = null;
        rpcSite.setConnected(false);
    }

    @Override
    protected void triggerShutdown() {
        stopped = true;
    }

    @Override
    protected void run() throws Exception {
        final OtpMbox eventBox = getEventMbox();
        do {
            receiveEventMessage(eventBox);
        } while (!stopped && !crashed);
        if (crashed && !stopped) {
            waitForExit();
        }
    }

    private void receiveEventMessage(final OtpMbox eventBox)
            throws OtpErlangExit {
        OtpErlangObject msg = null;
        eventHelper = new EventParser();
        try {
            msg = eventBox.receive(POLL_INTERVAL);
            final ErlEvent busEvent = eventHelper.parse(msg, this);
            if (busEvent != null) {
                if (DEBUG) {
                    ErlLogger.debug(
                            "MSG: %s",
                            "[" + busEvent.getSender() + "::"
                                    + busEvent.getTopic() + ": "
                                    + busEvent.getEvent() + "]");
                }
                eventBus.post(busEvent);
            }
        } catch (final OtpErlangExit e) {
            ErlLogger.error(e);
            throw e;
        } catch (final OtpErlangDecodeException e) {
            ErlLogger.error(e);
        }
    }

    @Override
    public String getNodeName() {
        return data.getQualifiedNodeName();
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
    public OtpMbox getEventMbox() {
        return eventMBox;
    }

    @Override
    public RuntimeData getRuntimeData() {
        return data;
    }

    @Override
    public void addShutdownCallback(final IShutdownCallback aCallback) {
        callback = aCallback;
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

    /**
     * @throws ErlRuntimeException
     */
    protected void waitForExit() throws ErlRuntimeException {
    }

    @Override
    public IRpcSite getRpcSite() {
        startAndWait();
        return rpcSite;
    }

    @Override
    public Process getProcess() {
        return null;
    }

    @Override
    public void registerEventListener(final Object handler) {
        eventBus.register(handler);
    }

    @Override
    protected String serviceName() {
        return getClass().getSimpleName() + " " + getNodeName();
    }

    private OtpNode startLocalNode() throws IOException {
        wait_for_epmd();
        final OtpNode lNode = createOtpNode(data.getCookie(),
                data.hasLongName());
        final OtpNodeStatus statusWatcher = new ErlideNodeStatus();
        lNode.registerStatusHandler(statusWatcher);
        return lNode;
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

    private String createJavaNodeName() {
        final String fUniqueId = getTimeSuffix();
        return "jerlide_" + fUniqueId;
    }

    private String createJavaNodeName(final String hostName) {
        return createJavaNodeName() + "@" + hostName;
    }

    private String getTimeSuffix() {
        String fUniqueId;
        fUniqueId = Long.toHexString(System.currentTimeMillis() & 0xFFFFFFF);
        return fUniqueId;
    }

    private OtpNode createOtpNode(final String cookie, final boolean longName)
            throws IOException {
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

    private void connect() throws Exception {
        final String label = getNodeName();
        ErlLogger.debug(label + ": waiting connection to peer...");
        try {
            pingPeer();
            int i = 0;
            while (state() == State.NEW && i++ < 20) {
                try {
                    Thread.sleep(POLL_INTERVAL);
                } catch (final InterruptedException e) {
                }
            }
        } catch (final Exception e) {
            ErlLogger.error(COULD_NOT_CONNECT, getNodeName());
            throw e;
        }
    }

    private void wait_for_epmd() {
        wait_for_epmd(null);
    }

    private void wait_for_epmd(final String host) {
        // If anyone has a better solution for waiting for epmd to be up, please
        // let me know
        int tries = 30;
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
                Thread.sleep(POLL_INTERVAL);
            } catch (final InterruptedException e1) {
            }
            tries--;
        } while (!ok && tries > 0);
        if (!ok) {
            final String msg = "Couldn't contact epmd - erlang backend is probably not working\n"
                    + "Your host's entry in /etc/hosts is probably wrong ("
                    + host + ").";
            ErlLogger.error(msg);
            throw new RuntimeException(msg);
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

    private class ErlideNodeStatus extends OtpNodeStatus {
        public ErlideNodeStatus() {
            // TODO Auto-generated constructor stub
        }

        @Override
        public void remoteStatus(final String node, final boolean up,
                final Object info) {
            if (node.equals(getNodeName()) && !up) {
                triggerCrashed();
            }
        }
    }

    @Subscribe
    public void deadEventHandler(final DeadEvent dead) {
        ErlLogger.warn("Dead event: " + dead + " in runtime " + getNodeName());
    }

    protected void triggerCrashed() {
        rpcSite.setConnected(false);
        crashed = true;
    }

    private class ErlRuntimeListener implements Listener {
        public ErlRuntimeListener() {
            // TODO Auto-generated constructor stub
        }

        @Override
        public void terminated(final State from) {
            ErlLogger.debug(String.format(
                    "Runtime %s terminated, exit code: %d", getNodeName(),
                    getExitCode()));
            if (data.isReportErrors()) {
                reporter.reportRuntimeDown(getNodeName(), getSystemStatus());
            }
        }

        @Override
        public void failed(final State from, final Throwable failure) {
            ErlLogger.warn(String.format("Runtime %s crashed, exit code: %d",
                    getNodeName(), getExitCode()));
            try {
                reporter.createFileReport(getNodeName(), getExitCode(),
                        getRuntimeData().getWorkingDir(), getSystemStatus());
            } catch (final Exception t) {
                ErlLogger.warn(t);
            }
        }

        @Override
        public void starting() {
            ErlLogger.debug("Runtime %s starting", getNodeName());
        }

        @Override
        public void running() {
            ErlLogger.debug("Runtime %s running", getNodeName());
        }

        @Override
        public void stopping(final State from) {
            ErlLogger.debug("Runtime %s stopping", getNodeName());
        }
    }

    public int getExitCode() {
        return -1;
    }

}
