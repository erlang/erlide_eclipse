package org.erlide.runtime.debug;

import java.util.List;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.IBackendListener;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;
import com.google.common.collect.Lists;

/**
 * This daemon is almost a copy of EventDaemon, but accepts generic messages
 * because the debugger can't be convinced to send erlide_jrpc events. The
 * handler is also hardcoded.
 */
public class DebuggerEventDaemon implements IBackendListener {

    private Backend runtime;
    volatile boolean stopped = false;
    private final DebugEventHandler handler;
    private OtpMbox mbox;

    final static boolean DEBUG = "true".equals(System
            .getProperty("erlide.event.daemon"));

    private final class HandlerJob implements Runnable {
        private final Backend backend;

        public HandlerJob(final Backend backend) {
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

    public DebuggerEventDaemon(final Backend b, ErlangDebugTarget target) {
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

    public void runtimeAdded(final Backend b) {
    }

    public void runtimeRemoved(final Backend b) {
        if (b == runtime) {
            stop();
            runtime = null;
        }
    }

    public void moduleLoaded(final Backend backend, final String projectName,
            final String moduleName) {
    }

    public OtpErlangPid getMBox() {
        return mbox.self();
    }
}
