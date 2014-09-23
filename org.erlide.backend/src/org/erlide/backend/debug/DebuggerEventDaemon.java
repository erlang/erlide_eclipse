package org.erlide.backend.debug;

import java.util.List;

import org.eclipse.core.resources.IProject;
import org.erlide.backend.api.IBackend;
import org.erlide.backend.api.IBackendListener;
import org.erlide.backend.debug.model.ErlangDebugTarget;
import org.erlide.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
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

    private IBackend backend;
    volatile boolean stopped = false;
    private final DebugEventHandler handler;
    private OtpMbox mbox;

    final static boolean DEBUG = Boolean.parseBoolean(System
            .getProperty("erlide.event.daemon"));

    private final class HandlerJob implements Runnable {
        private final IBackend myBackend;

        public HandlerJob(final IBackend backend) {
            myBackend = backend;
        }

        @Override
        public void run() {
            do {
                try {
                    final List<OtpErlangObject> messages = receiveSomeMessages(mbox);
                    if (messages.size() != 0) {
                        if (DEBUG) {
                            for (final OtpErlangObject message : messages) {
                                ErlLogger.debug("MSG: %s", message);
                            }
                        }
                        handler.handleMessages(messages);
                    }
                } catch (final OtpErlangExit e) {
                    if (myBackend.isRunning()) {
                        // backend crashed -- restart?
                        // also when it was closed... how do we tell difference?
                        // ErlLogger.warn(e);
                    }
                } catch (final Exception e) {
                    ErlLogger.warn(e);
                }
            } while (!stopped);
        }

        private List<OtpErlangObject> receiveSomeMessages(final OtpMbox box)
                throws OtpErlangExit, OtpErlangDecodeException {
            final List<OtpErlangObject> messages = Lists.newArrayList();
            OtpErlangObject msg = box.receive(200);
            if (msg != null) {
                messages.add(msg);
                // if there are more queued events, retrieve not
                // more than 10 of them
                int count = 0;
                do {
                    msg = box.receive(0);
                    if (msg != null) {
                        messages.add(msg);
                        count++;
                    }
                } while (count < 10 && msg != null && !stopped);
            }
            return messages;
        }
    }

    public DebuggerEventDaemon(final IBackend b, final ErlangDebugTarget target) {
        backend = b;
        handler = new DebugEventHandler(target);
    }

    public synchronized void start() {
        stopped = false;
        mbox = backend.getRuntime().createMbox();
        new Thread(new HandlerJob(backend)).start();
    }

    public synchronized void stop() {
        stopped = true;
    }

    @Override
    public void runtimeAdded(final IBackend b) {
    }

    @Override
    public void runtimeRemoved(final IBackend b) {
        if (b == backend) {
            stop();
            backend = null;
        }
    }

    @Override
    public void moduleLoaded(final IBackend aBackend, final IProject project,
            final String moduleName) {
    }

    public OtpErlangPid getMBox() {
        return mbox.self();
    }
}
