package org.erlide.core.backend.events;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.erlide.core.backend.BackendException;
import org.erlide.core.backend.IBackend;
import org.erlide.core.backend.IBackendListener;
import org.erlide.core.rpc.IRpcCallSite;
import org.erlide.jinterface.ErlLogger;
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

public class ErlangEventPublisher implements IBackendListener {

    private IBackend runtime;
    volatile boolean stopped = false;
    private EventAdmin eventAdmin;

    final static boolean DEBUG = Boolean.parseBoolean(System
            .getProperty("erlide.event.daemon"));

    public ErlangEventPublisher(final IBackend b) {
        runtime = b;
    }

    public synchronized void start() {
        stopped = false;
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

    private final class HandlerJob implements Runnable {
        private final IBackend backend;

        public HandlerJob(final IBackend backend) {
            this.backend = backend;
        }

        public void run() {
            OtpErlangObject msg = null;
            do {
                try {
                    msg = backend.receiveEvent(200);
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
                        publishEvent(backend, topic, data, sender);
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

    public void publishEvent(final IBackend backend, final String topic,
            final OtpErlangObject event, final OtpErlangPid sender) {

        final Map<String, Object> properties = new HashMap<String, Object>();
        properties.put("BACKEND", backend);
        properties.put("DATA", event);
        properties.put("SENDER", sender);

        final Event osgiEvent = new Event(getFullTopic(topic, backend),
                properties);
        getEventAdmin().postEvent(osgiEvent);
    }

    public static String getFullTopic(final String topic, final IBackend backend) {
        final String subtopic = "*".equals(topic) ? "" : "/"
                + (backend == null ? "*" : backend.getName()
                        .replaceAll("@", "__").replaceAll(".", "_"));
        return "erlideEvent/" + topic + subtopic;
    }

    private EventAdmin getEventAdmin() {
        if (eventAdmin == null) {
            final BundleContext ctx = FrameworkUtil.getBundle(
                    ErlangEventPublisher.class).getBundleContext();
            final ServiceReference ref = ctx
                    .getServiceReference(EventAdmin.class.getName());
            if (ref == null) {
                ErlLogger.error("No event admin ???");
            } else {
                eventAdmin = (EventAdmin) ctx.getService(ref);
            }
        }
        return eventAdmin;
    }

    public static String dumpEvent(final Event event) {
        String result = "";
        for (final String key : event.getPropertyNames()) {
            result += key + ":" + event.getProperty(key) + ", ";
        }
        return result;
    }

}
