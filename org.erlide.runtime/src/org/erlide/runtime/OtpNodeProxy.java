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
package org.erlide.runtime;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.erlide.runtime.api.IOtpNodeProxy;
import org.erlide.runtime.api.IOtpRpc;
import org.erlide.runtime.api.IShutdownCallback;
import org.erlide.runtime.api.RuntimeData;
import org.erlide.runtime.events.ErlEvent;
import org.erlide.runtime.events.ErlangLogEventHandler;
import org.erlide.runtime.events.LogEventHandler;
import org.erlide.runtime.internal.ErlRuntimeException;
import org.erlide.runtime.internal.ErlRuntimeReporter;
import org.erlide.runtime.internal.EventParser;
import org.erlide.runtime.internal.LocalNodeCreator;
import org.erlide.runtime.internal.rpc.OtpRpc;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import com.google.common.eventbus.DeadEvent;
import com.google.common.eventbus.EventBus;
import com.google.common.eventbus.Subscribe;
import com.google.common.util.concurrent.AbstractExecutionThreadService;

public class OtpNodeProxy extends AbstractExecutionThreadService implements IOtpNodeProxy {
    private static final String COULD_NOT_CONNECT = "Could not connect to %s! Please check runtime settings.";

    private static final int MAX_RETRIES = 15;
    public static final int RETRY_DELAY = Integer.parseInt(System.getProperty(
            "erlide.connect.delay", "400"));

    protected final RuntimeData data;
    private OtpNode localNode;
    final ErlRuntimeReporter reporter;
    private OtpMbox eventMBox;
    private IShutdownCallback callback;
    private IOtpRpc OtpRpc;
    private final EventBus eventBus;
    protected volatile boolean stopped;
    private final EventParser eventHelper;
    boolean crashed;

    static final boolean DEBUG = Boolean.parseBoolean(System
            .getProperty("erlide.event.daemon"));
    public static final long POLL_INTERVAL = 100;

    public OtpNodeProxy(final RuntimeData data) {
        this.data = data;
        reporter = new ErlRuntimeReporter(data.isInternal());

        eventHelper = new EventParser();
        final String nodeName = getNodeName();
        eventBus = new EventBus(nodeName);
        eventBus.register(this);
        registerEventListener(new LogEventHandler(nodeName));
        registerEventListener(new ErlangLogEventHandler(nodeName));

        addListener(new ErlRuntimeListener(), executor());
    }

    @Override
    protected void startUp() throws Exception {
        localNode = LocalNodeCreator.startLocalNode(this, data.getCookie(),
                data.hasLongName());
        eventMBox = createMbox("rex");
        OtpRpc = new OtpRpc(this, localNode, getNodeName());
        connect();
        OtpRpc.setConnected(true);

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
        OtpRpc.setConnected(false);
    }

    @Override
    protected void triggerShutdown() {
        stopped = true;
    }

    @Override
    protected void run() throws Exception {
        final OtpMbox eventBox = eventMBox;
        do {
            receiveEventMessage(eventBox);
        } while (!stopped && !crashed);
        if (crashed && !stopped) {
            waitForExit();
        }
    }

    private void receiveEventMessage(final OtpMbox eventBox) throws OtpErlangExit {
        OtpErlangObject msg = null;
        try {
            msg = eventBox.receive(POLL_INTERVAL);
            final ErlEvent busEvent = eventHelper.parse(msg, this);
            if (busEvent != null) {
                if (DEBUG) {
                    ErlLogger.debug("MSG: %s", "[" + busEvent.getSender() + "::"
                            + busEvent.getTopic() + ": " + busEvent.getEvent() + "]");
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
    public OtpErlangPid getEventPid() {
        return eventMBox.self();
    }

    @Override
    public RuntimeVersion getVersion() {
        return data.getRuntimeInfo().getVersion();
    }

    @Override
    public String getOtpHome() {
        return data.getRuntimeInfo().getOtpHome();
    }

    @Override
    public void setShutdownCallback(final IShutdownCallback aCallback) {
        callback = aCallback;
    }

    @Override
    public void dispose() {
        triggerShutdown();
    }

    @SuppressWarnings("unused")
    protected void waitForExit() throws ErlRuntimeException {
    }

    @Override
    public IOtpRpc getOtpRpc() {
        try {
            awaitTerminated(20, TimeUnit.MILLISECONDS);
            return null;
        } catch (final TimeoutException e) {
            awaitRunning();
            return OtpRpc;
        }
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

    private void connect() throws Exception {
        final String label = getNodeName();
        ErlLogger.debug(label + ": waiting connection to peer... ");

        final boolean connected = pingPeer();
        if (!connected) {
            ErlLogger.error(COULD_NOT_CONNECT, getNodeName());
            throw new Exception(COULD_NOT_CONNECT);
        }
    }

    private boolean pingPeer() {
        int tries = MAX_RETRIES;
        boolean ok = false;
        while (!ok && tries > 0) {
            ok = localNode.ping(getNodeName(), RETRY_DELAY + (MAX_RETRIES - tries)
                    * RETRY_DELAY % 3);
            tries--;
        }
        return ok;
    }

    private boolean waitForCodeServer() {
        try {
            OtpErlangObject r;
            int i = 30;
            boolean gotIt = false;
            do {
                r = OtpRpc.call("erlang", "whereis", "a", "code_server");
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
                ErlLogger
                        .error("code server did not start in time for %s", getNodeName());
                return false;
            }
            return true;
        } catch (final Exception e) {
            ErlLogger.error("error starting code server for %s: %s", getNodeName(),
                    e.getMessage());
            return false;
        }
    }

    @Subscribe
    public void deadEventHandler(final DeadEvent dead) {
        ErlLogger.warn("Dead event: " + dead + " in runtime " + getNodeName());
    }

    public void triggerCrashed() {
        OtpRpc.setConnected(false);
        crashed = true;
    }

    private class ErlRuntimeListener extends Listener {
        public ErlRuntimeListener() {
        }

        @Override
        public void terminated(final State from) {
            ErlLogger.debug(String.format("Runtime %s terminated", getNodeName()));
            dispose();
            reportDown();
        }

        @Override
        public void failed(final State from, final Throwable failure) {
            final String nodeName = getNodeName();
            final int exitCode = getExitCode();
            ErlLogger.warn(String.format("Runtime %s crashed, exit code: %d.", nodeName,
                    exitCode));
            dispose();
            reportDown();
            try {
                if (data.isReportErrors()) {
                    reporter.createFileReport(nodeName, exitCode);
                }
            } catch (final Exception t) {
                ErlLogger.warn(t);
            }
        }

        private void reportDown() {
            if (data.isReportErrors() && getExitCode() > 0) {
                reporter.reportRuntimeDown(getNodeName());
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

    @Override
    public void startAndWait() {
        startAsync();
        awaitRunning();
    }

}
