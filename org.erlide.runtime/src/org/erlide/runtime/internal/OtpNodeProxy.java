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
import java.util.Arrays;
import java.util.Map;
import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.erlide.runtime.api.IOtpNodeProxy;
import org.erlide.runtime.api.RuntimeData;
import org.erlide.runtime.events.ErlEvent;
import org.erlide.runtime.events.ErlangLogEventHandler;
import org.erlide.runtime.events.LogEventHandler;
import org.erlide.runtime.internal.rpc.OtpRpc;
import org.erlide.runtime.rpc.IOtpRpc;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.runtime.service.CooldownRestartPolicy;
import org.erlide.runtime.service.NeverRestartPolicy;
import org.erlide.runtime.service.RestartableService;
import org.erlide.runtime.service.ServiceRestartPolicy;
import org.erlide.util.ErlLogger;
import org.erlide.util.SystemConfiguration;
import org.erlide.util.services.Provider;

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
import com.google.common.util.concurrent.MoreExecutors;
import com.google.common.util.concurrent.Service;
import com.google.common.util.concurrent.Service.Listener;
import com.google.common.util.concurrent.Service.State;

public class OtpNodeProxy implements IOtpNodeProxy {
    private static final String COULD_NOT_CONNECT = "Could not connect to %s! Please check runtime settings.";
    private static final long EXIT_POLL_INTERVAL = 500;

    private static final int MAX_RETRIES = 15;
    public static final int RETRY_DELAY = Integer
            .parseInt(System.getProperty("erlide.connect.delay", "400"));

    private final Service service;
    protected final RuntimeData data;
    private OtpNode localNode = null;
    final ErlRuntimeReporter reporter;
    private OtpMbox eventMBox;
    private IOtpRpc otpRpc;
    private final EventBus eventBus;
    private final EventParser eventHelper;
    private Process process = null;
    private volatile int exitCode = -1;

    static final boolean DEBUG = Boolean
            .parseBoolean(System.getProperty("erlide.event.daemon"));
    public static final long POLL_INTERVAL = 100;

    public OtpNodeProxy(final RuntimeData data) {
        this.data = data;
        reporter = new ErlRuntimeReporter(data.isInternal());

        eventHelper = new EventParser();
        final String nodeName = getNodeName();
        eventBus = new EventBus(nodeName);
        eventBus.register(this);
        registerEventListener(new LogEventHandler());
        registerEventListener(new ErlangLogEventHandler());

        final Provider<Service> factory = new Provider<Service>() {
            @Override
            public Service get() {
                return new OtpNodeProxyService(data.isManaged());
            }
        };

        ServiceRestartPolicy policy;
        if (data.isRestartable()) {
            policy = new CooldownRestartPolicy(6000);
        } else {
            policy = new NeverRestartPolicy();
        }

        service = new RestartableService(factory, policy);
        service.addListener(new ErlRuntimeListener(), MoreExecutors.sameThreadExecutor());
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
    public void dispose() {
        otpRpc.setConnected(false);
        localNode.close();
        service.stopAsync();
    }

    protected void waitForExit() {
        if (process != null) {
            int i = 500;
            // may have to wait for crash dump to be written
            while (i-- > 0 && exitCode < 0) {
                exitCode = -1;
                try {
                    Thread.sleep(EXIT_POLL_INTERVAL);
                    exitCode = process.exitValue();
                } catch (final IllegalThreadStateException e) {
                } catch (final InterruptedException e) {
                }
                if (exitCode > 0) {
                    // throw new ErlRuntimeException(String.format(
                    // "Runtime %s died with exit code %d", getNodeName(),
                    // exitCode));
                }
            }
            if (exitCode < 0) {
                ErlLogger.warn(
                        "Runtime %s died, but process is still running; killing it",
                        getNodeName());
                // throw new ErlRuntimeException(String.format(
                // "Runtime %s died with exit code unknown", getNodeName()));
            }
        }
    }

    @Override
    public IOtpRpc getOtpRpc() {
        try {
            service.awaitTerminated(20, TimeUnit.MILLISECONDS);
            return null;
        } catch (final TimeoutException e) {
            service.awaitRunning();
            return otpRpc;
        }
    }

    @Override
    public Process getProcess() {
        service.awaitRunning();
        return process;
    }

    @Override
    public void registerEventListener(final Object handler) {
        eventBus.register(handler);
    }

    private void connect() throws Exception {
        final String label = getNodeName();
        ErlLogger.debug(label + ": waiting connection to peer... ");

        final boolean connected = pingPeer();
        if (!connected) {
            ErlLogger.error(COULD_NOT_CONNECT, label);
            throw new Exception(String.format(COULD_NOT_CONNECT, label));
        }
        ErlLogger.debug("connected!");
    }

    private boolean pingPeer() {
        int tries = MAX_RETRIES;
        boolean ok = false;
        while (!ok && tries > 0) {
            ok = localNode.ping(getNodeName(),
                    RETRY_DELAY + (MAX_RETRIES - tries) * RETRY_DELAY % 3);
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
                r = otpRpc.call("erlang", "whereis", "a", "code_server");
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

    private class ErlRuntimeListener extends Listener {

        @Override
        public void terminated(final State from) {
            ErlLogger.debug(String.format("Runtime %s terminated", getNodeName()));
            dispose();
            reportDown();
        }

        @Override
        public void failed(final State from, final Throwable failure) {
            final String nodeName = getNodeName();
            final int myExitCode = getExitCode();
            ErlLogger.warn(String.format("Runtime %s crashed, exit code: %d.", nodeName,
                    myExitCode));
            dispose();
            reportDown();
            try {
                if (data.isReportErrors()) {
                    reporter.createFileReport(nodeName, myExitCode);
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
        return exitCode;
    }

    @Override
    public void ensureRunning() {
        service.startAsync().awaitRunning();
        // we want to make sure backend was initialized!
        try {
            Thread.sleep(500);
        } catch (final InterruptedException e) {
        }

    }

    @Override
    public boolean isRunning() {
        return service.isRunning();
    }

    @Override
    public void addRuntimeListener(final Listener listener, final Executor executor) {
        service.addListener(listener, executor);
    }

    private class OtpNodeProxyService extends AbstractExecutionThreadService {

        private final boolean managed;
        protected volatile boolean stopped;
        boolean crashed;

        public OtpNodeProxyService(final boolean managed) {
            this.managed = managed;
        }

        @Override
        protected void startUp() throws Exception {
            if (managed) {
                exitCode = -1;
                process = startRuntimeProcess(data);
                if (process == null) {
                    throw new Exception("no runtime");
                }
            }
            initialize();

            connect();
            otpRpc.setConnected(true);

            if (!waitForCodeServer()) {
                // crash?
                triggerShutdown();
                ErlLogger.error(COULD_NOT_CONNECT, getNodeName());
            }
            stopped = false;
            crashed = false;
        }

        @Override
        protected void shutDown() throws Exception {
            if (crashed && !stopped) {
                waitForExit();
            }
            otpRpc.setConnected(false);
            if (managed) {
                process.destroy();
                process = null;
            }
        }

        @Override
        protected void triggerShutdown() {
            stopped = true;
        }

        @Override
        protected void run() throws Exception {
            final OtpMbox eventBox = eventMBox;
            int theCode = -1;
			do {
                receiveEventMessage(eventBox);
                if (managed) {
                    try {
                        final int code = process.exitValue();
                        stopped = code == 0;
                        crashed = code != 0;
                        theCode = code;
                    } catch (final IllegalThreadStateException e) {
                    }
                }
            } while (!stopped && !crashed);
            if (crashed) {
				throw new Exception("Runtime crashed " + theCode+" : "+getNodeName());
            }
        }

        @Override
        protected String serviceName() {
            return "OtpNodeProxy " + getNodeName();
        }

        private Process startRuntimeProcess(final RuntimeData rtData) {
            final String[] cmds = rtData.getCmdLine();
            final File workingDirectory = new File(rtData.getWorkingDir());

            try {
                ErlLogger.debug("START node :> " + Arrays.toString(cmds) + " *** "
                        + workingDirectory.getCanonicalPath());
            } catch (final IOException e1) {
                ErlLogger.error("START ERROR node :> " + e1.getMessage());
            }

            final ProcessBuilder builder = new ProcessBuilder(cmds);
            builder.directory(workingDirectory);
            setEnvironment(rtData, builder);
            try {
                final Process aProcess = builder.start();
                return aProcess;
            } catch (final IOException e) {
                ErlLogger.error("Could not create runtime: %s", Arrays.toString(cmds));
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

    }

    public State state() {
        return service.state();
    }

    public void initialize() {
        if (localNode == null) {
            localNode = LocalNodeCreator.startLocalNode(OtpNodeProxy.this,
                    data.getCookie(), data.hasLongName());
            eventMBox = createMbox("rex");
            otpRpc = new OtpRpc(localNode, getNodeName());
        }
    }

}
