package org.erlide.jinterface.backend.events;

import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

public final class ErlangEvent {
    public final Backend backend;
    public final String topic;
    public final OtpErlangObject data;
    public final OtpErlangPid sender;

    public static ErlangEvent parseEvent(Backend backend,
            OtpErlangObject message) throws BackendException {
        if (!isEventMessage(message)) {
            throw new BackendException("Bad event data " + message);
        }
        String topic = getEventTopic(message);
        OtpErlangObject data = getEventData(message);
        OtpErlangPid sender = getEventSender(message);
        return new ErlangEvent(backend, topic, data, sender);
    }

    private ErlangEvent(Backend backend, String topic, OtpErlangObject data,
            OtpErlangPid sender) {
        if (topic == null) {
            throw new IllegalArgumentException("topic can't be null");
        }
        if (data == null) {
            throw new IllegalArgumentException("data can't be null");
        }
        this.backend = backend;
        this.topic = topic;
        this.data = data;
        this.sender = sender;
    }

    @Override
    public String toString() {
        return "[" + backend.getName() + ":" + topic + ": " + data + "]";
    }

    private static OtpErlangPid getEventSender(final OtpErlangObject msg) {
        final OtpErlangTuple tmsg = (OtpErlangTuple) msg;
        return (OtpErlangPid) tmsg.elementAt(3);
    }

    private static OtpErlangObject getEventData(final OtpErlangObject msg) {
        final OtpErlangTuple tmsg = (OtpErlangTuple) msg;
        return tmsg.elementAt(2);
    }

    private static String getEventTopic(final OtpErlangObject msg) {
        final OtpErlangTuple tmsg = (OtpErlangTuple) msg;
        final Object el0 = tmsg.elementAt(1);
        final OtpErlangAtom a = (OtpErlangAtom) el0;
        return a.atomValue();
    }

    private static boolean isEventMessage(final OtpErlangObject msg) {
        try {
            final OtpErlangTuple tmsg = (OtpErlangTuple) msg;
            final OtpErlangObject el0 = tmsg.elementAt(0);
            return ((OtpErlangAtom) el0).atomValue().equals("event")
                    && tmsg.arity() == 4;
        } catch (Exception e) {
            return false;
        }
    }

    public boolean hasTopic(String string) {
        return topic.equals(string);
    }

}
