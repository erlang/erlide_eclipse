package org.erlide.core.debug;

import java.util.List;

import org.eclipse.core.resources.IProject;
import org.erlide.core.backend.IBackend;
import org.erlide.core.backend.IBackendListener;
import org.erlide.core.rpc.IRpcCallSite;
import org.erlide.jinterface.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;
import com.google.common.collect.Lists;

/**
 * This daemon is almost a copy of ErlangEventPublisher, but accepts generic
 * messages because the debugger can't be convinced to send erlide_jrpc events.
 * The handler is also hardcoded.
 */
public class DebuggerEventDaemon implements IBackendListener {

    private IBackend runtime;
    volatile boolean stopped = false;
    private final DebugEventHandler handler;
    private OtpMbox mbox;

    final static boolean DEBUG = Boolean.parseBoolean(System
            .getProperty("erlide.event.daemon"));

    private final class HandlerJob implements Runnable {
        private final IBackend backend;

        public HandlerJob(final IBackend backend) {
            this.backend = backend;
        }

        public void run() {
            OtpErlangObject msg = null;
            final List<OtpErlangObject> messages = Lists.newArrayList();
            do {
                try {
                    msg = mbox.receive(200);
                    if (msg != null) {
                        messages.add(msg);
                        // if there are more queued events, retrieve not
                        // more than 10 of them
                        int count = 0;
                        do {
                            msg = mbox.receive(0);
                            if (msg != null) {
                                messages.add(msg);
                                count++;
                            }
                        } while (count < 10 && msg != null && !stopped);
                    }
                    if (messages.size() != 0) {
                        if (DEBUG) {
                            for (final OtpErlangObject message : messages) {
                                ErlLogger.debug("MSG: %s", message);
                            }
                        }

                        handler.handleMessages(messages);

                        messages.clear();
                    }
                } catch (final OtpErlangExit e) {
                    if (!backend.isStopped()) {
                        // backend crashed -- restart?
                        ErlLogger.warn(e);
                    }
                } catch (final Exception e) {
                    ErlLogger.warn(e);
                }
            } while (!stopped);
        }
    }

    public DebuggerEventDaemon(final IBackend b, final ErlangDebugTarget target) {
        runtime = b;
        handler = new DebugEventHandler(target);
    }

    public synchronized void start() {
        stopped = false;
        mbox = runtime.createMbox();
        new Thread(new HandlerJob(runtime)).start();
    }

    public synchronized void stop() {
        stopped = true;
    }

    public void runtimeAdded(final IBackend b) {
    }

    public void runtimeRemoved(final IBackend b) {
        if (b == runtime) {
            stop();
            runtime = null;
        }
    }

    public void moduleLoaded(final IRpcCallSite backend,
            final IProject project, final String moduleName) {
    }

    public OtpErlangPid getMBox() {
        return mbox.self();
    }
}
