package org.erlide.backend.events;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.erlide.backend.BackendException;
import org.erlide.backend.IBackend;
import org.erlide.backend.IBackendListener;
import org.erlide.runtime.api.IErlRuntime;
import org.erlide.util.ErlLogger;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.framework.ServiceReference;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventAdmin;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;

public class ErlangEventPublisher {

    private volatile IErlRuntime backend;
    volatile boolean stopped = false;
    private EventAdmin eventAdmin;
    private final IBackendListener backendListener;

    final static boolean DEBUG = Boolean.parseBoolean(System
            .getProperty("erlide.event.daemon"));

    public ErlangEventPublisher(final IErlRuntime aBackend) {
        backend = aBackend;
        backendListener = new IBackendListener() {

            @Override
            public void runtimeAdded(final IBackend b) {
            }

            @Override
            public void runtimeRemoved(final IBackend b) {
                if (b == aBackend) {
                    stop();
                    backend = null;
                }
            }

            @Override
            public void moduleLoaded(final IBackend b, final IProject project,
                    final String moduleName) {
            }

        };
    }

    public synchronized void start() {
        stopped = false;
        setEventAdmin();
        new Thread(new HandlerJob(backend)).start();
    }

    public void setEventAdmin() {
        final BundleContext ctx = FrameworkUtil.getBundle(
                ErlangEventPublisher.class).getBundleContext();
        @SuppressWarnings("unchecked")
        final ServiceReference<EventAdmin> ref = (ServiceReference<EventAdmin>) ctx
                .getServiceReference(EventAdmin.class.getName());
        if (ref == null) {
            ErlLogger.error("No event admin ???");
        } else {
            eventAdmin = ctx.getService(ref);
        }
    }

    public synchronized void stop() {
        stopped = true;
        unsetEventAdmin();
    }

    public void unsetEventAdmin() {
        final BundleContext ctx = FrameworkUtil.getBundle(
                ErlangEventPublisher.class).getBundleContext();
        if (ctx == null) {
            return;
        }
        @SuppressWarnings("unchecked")
        final ServiceReference<EventAdmin> ref = (ServiceReference<EventAdmin>) ctx
                .getServiceReference(EventAdmin.class.getName());
        if (ref == null) {
            ErlLogger.error("No event admin ???");
        } else {
            ctx.ungetService(ref);
        }
    }

    private final class HandlerJob implements Runnable {
        private final IErlRuntime myBackend;

        public HandlerJob(final IErlRuntime backend) {
            myBackend = backend;
        }

        @Override
        public void run() {
            OtpErlangObject msg = null;
            do {
                try {
                    final OtpMbox eventBox = myBackend.getEventMbox();
                    msg = eventBox != null ? eventBox.receive(200) : null;
                    String topic = null;
                    OtpErlangObject data = null;
                    OtpErlangPid sender = null;
                    if (msg != null) {
                        if (!isEventMessage(msg)) {
                            throw new BackendException("Bad event data " + msg);
                        }
                        topic = getEventTopic(msg);
                        data = getEventData(msg);
                        sender = getEventSender(msg);
                    }
                    if (topic != null) {
                        if (DEBUG) {
                            ErlLogger.debug("MSG: %s", "[" + sender + "::"
                                    + topic + ": " + data + "]");
                        }
                        publishEvent(myBackend, topic, data, sender);
                    }
                } catch (final OtpErlangExit e) {
                    if (!myBackend.isStopped()) {
                        // backend crashed -- restart?
                        ErlLogger.warn(e);
                    }
                } catch (final Exception e) {
                    ErlLogger.warn(e);
                }
            } while (!stopped);
        }

        private boolean isEventMessage(final OtpErlangObject msg) {
            try {
                final OtpErlangTuple tmsg = (OtpErlangTuple) msg;
                final OtpErlangObject el0 = tmsg.elementAt(0);
                return ((OtpErlangAtom) el0).atomValue().equals("event")
                        && tmsg.arity() == 4;
            } catch (final Exception e) {
                return false;
            }
        }

        private OtpErlangPid getEventSender(final OtpErlangObject msg) {
            final OtpErlangTuple tmsg = (OtpErlangTuple) msg;
            return (OtpErlangPid) tmsg.elementAt(3);
        }

        private OtpErlangObject getEventData(final OtpErlangObject msg) {
            final OtpErlangTuple tmsg = (OtpErlangTuple) msg;
            return tmsg.elementAt(2);
        }

        private String getEventTopic(final OtpErlangObject msg) {
            final OtpErlangTuple tmsg = (OtpErlangTuple) msg;
            final Object el0 = tmsg.elementAt(1);
            final OtpErlangAtom a = (OtpErlangAtom) el0;
            return a.atomValue();
        }
    }

    public void publishEvent(final IErlRuntime b, final String topic,
            final OtpErlangObject event, final OtpErlangPid sender) {

        final Map<String, Object> properties = new HashMap<String, Object>();
        properties.put("BACKEND", b);
        properties.put("DATA", event);
        properties.put("SENDER", sender);

        final Event osgiEvent = new Event(getFullTopic(topic, b.getName()),
                properties);
        eventAdmin.postEvent(osgiEvent);
    }

    public static String getFullTopic(final String topic,
            final String backendName) {
        final String subtopic = "*".equals(topic) ? "" : "/"
                + (backendName == null ? "*" : backendName
                        .replaceAll("@", "__").replaceAll("\\.", "_"));
        return "erlideEvent/" + topic + subtopic;
    }

    public static String dumpEvent(final Event event) {
        String result = "";
        for (final String key : event.getPropertyNames()) {
            result += key + ":" + event.getProperty(key) + ", ";
        }
        return result;
    }

    public IBackendListener getBackendListener() {
        return backendListener;
    }

}
