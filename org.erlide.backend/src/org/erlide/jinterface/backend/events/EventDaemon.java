package org.erlide.jinterface.backend.events;

import java.util.ArrayList;
import java.util.List;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.IBackendListener;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Lists;

public class EventDaemon implements IBackendListener {

    private Backend runtime;
    volatile boolean stopped = false;
    List<EventHandler> handlers = new ArrayList<EventHandler>();
    final Object handlersLock = new Object();

    final static boolean DEBUG = "true".equals(System
            .getProperty("erlide.event.daemon"));

    public EventDaemon(final Backend b) {
        runtime = b;
    }

    public synchronized void start() {
        stopped = false;
        new Thread(new HandlerJob(runtime)).start();
        // addHandler(new RpcHandler(runtime));
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

    public List<EventHandler> getHandlers() {
        synchronized (handlersLock) {
            return new ArrayList<EventHandler>(handlers);
        }
    }

    public void addHandler(final EventHandler l) {
        synchronized (handlersLock) {
            if (!handlers.contains(l)) {
                handlers.add(l);
            }
        }
    }

    public void removeHandler(final EventHandler l) {
        synchronized (handlersLock) {
            handlers.remove(l);
        }
    }

    public void moduleLoaded(final Backend backend, final String projectName,
            final String moduleName) {
    }

    private final class HandlerJob implements Runnable {
        private final Backend backend;

        public HandlerJob(final Backend backend) {
            this.backend = backend;
        }

        public void run() {
            try {
                OtpErlangObject msg = null;
                final List<ErlangEvent> events = Lists.newArrayList();
                do {
                    try {
                        msg = backend.receiveEvent(200);
                        if (msg != null) {
                            events.add(ErlangEvent.parseEvent(msg));
                            // if there are more queued events, retrieve not
                            // more than 10 of them
                            int count = 0;
                            do {
                                msg = backend.receiveEvent(0);
                                if (msg != null) {
                                    events.add(ErlangEvent.parseEvent(msg));
                                    count++;
                                }
                            } while (count < 10 && msg != null && !stopped);
                        }
                        if (events.size() != 0) {
                            if (DEBUG) {
                                for (final ErlangEvent event : events) {
                                    ErlLogger.debug("MSG: %s", event);
                                }
                            }
                            for (final EventHandler handler : getHandlers()) {
                                handler.handleEvents(events);
                            }
                            events.clear();
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
            } finally {
                synchronized (handlersLock) {
                    handlers.clear();
                }
            }
        }
    }

}
