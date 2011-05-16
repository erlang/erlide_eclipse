package org.erlide.core.backend.events;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.erlide.core.backend.BackendListener;
import org.erlide.core.backend.IBackend;
import org.erlide.core.rpc.IRpcCallSite;
import org.erlide.jinterface.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Lists;

public class EventDaemon implements BackendListener {

    private IBackend runtime;
    volatile boolean stopped = false;
    List<EventHandler> handlers = new ArrayList<EventHandler>();
    final Object handlersLock = new Object();
    final List<ErlangEvent> events = Lists.newArrayList();

    final static boolean DEBUG = "true".equals(System
            .getProperty("erlide.event.daemon"));

    public EventDaemon(final IBackend b) {
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

    public void runtimeAdded(final IBackend b) {
    }

    public void runtimeRemoved(final IBackend b) {
        if (b == runtime) {
            stop();
            runtime = null;
        }
    }

    public Collection<EventHandler> getHandlers() {
        synchronized (handlersLock) {
            return Collections
                    .unmodifiableCollection(new ArrayList<EventHandler>(
                            handlers));
        }
    }

    public void addHandler(final EventHandler handler) {
        synchronized (handlersLock) {
            if (!handlers.contains(handler)) {
                handlers.add(handler);
            }
        }
    }

    public void removeHandler(final EventHandler handler) {
        synchronized (handlersLock) {
            handlers.remove(handler);
        }
    }

    public void moduleLoaded(final IRpcCallSite backend, final IProject project,
            final String moduleName) {
    }

    private final class HandlerJob implements Runnable {
        private final IBackend backend;

        public HandlerJob(final IBackend backend) {
            this.backend = backend;
        }

        public void run() {
            try {
                OtpErlangObject msg = null;
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

    public void postEvent(final String topic, final OtpErlangObject event) {
        events.add(new ErlangEvent(topic, event, null));
    }

}
