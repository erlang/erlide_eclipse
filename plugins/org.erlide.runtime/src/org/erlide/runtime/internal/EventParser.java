package org.erlide.runtime.internal;

import org.erlide.runtime.api.IOtpNodeProxy;
import org.erlide.runtime.events.ErlEvent;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class EventParser {

    public ErlEvent parse(final OtpErlangObject msg, final IOtpNodeProxy runtime) {
        if (msg == null) {
            return null;
        }
        final String topic = getEventTopic(msg);
        if (topic == null) {
            return null;
        }
        final OtpErlangObject event = getEventData(msg);
        final OtpErlangPid sender = getEventSender(msg);
        return new ErlEvent(topic, runtime, event, sender);
    }

    public boolean isEventMessage(final OtpErlangObject msg) {
        try {
            final OtpErlangTuple tmsg = (OtpErlangTuple) msg;
            final OtpErlangObject el0 = tmsg.elementAt(0);
            return ((OtpErlangAtom) el0).atomValue().equals("event") && tmsg.arity() == 4;
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
