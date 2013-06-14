package org.erlide.runtime.events;

import org.erlide.runtime.api.IErlRuntime;
import org.erlide.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.google.common.eventbus.DeadEvent;
import com.google.common.eventbus.EventBus;
import com.google.common.eventbus.Subscribe;

public class ErlangEventPublisher {

    private volatile IErlRuntime runtime;
    volatile boolean stopped = false;
    private final EventBus eventBus;

    final static boolean DEBUG = Boolean.parseBoolean(System
            .getProperty("erlide.event.daemon"));

    public ErlangEventPublisher(final IErlRuntime runtime) {
        eventBus = new EventBus(runtime.getNodeName());
        this.runtime = runtime;
        eventBus.register(this);
    }

    public synchronized void start() {
        stopped = false;
        new Thread(new HandlerJob(runtime)).start();
    }

    public synchronized void stop() {
        stopped = true;
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
                            // throw new Exception("Bad event data " + msg);
                            continue;
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
        final ErlEvent busEvent = new ErlEvent(topic, b, event, sender);
        eventBus.post(busEvent);
    }

    public void register(final Object handler) {
        eventBus.register(handler);
    }

    @Subscribe
    public void deadEventHandler(final DeadEvent dead) {
        ErlLogger.warn("Dead event: " + dead + " in " + runtime.getNodeName());
    }

}
