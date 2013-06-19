package org.erlide.runtime.internal;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class EventHelper {

    public boolean isEventMessage(final OtpErlangObject msg) {
        try {
            final OtpErlangTuple tmsg = (OtpErlangTuple) msg;
            final OtpErlangObject el0 = tmsg.elementAt(0);
            return ((OtpErlangAtom) el0).atomValue().equals("event")
                    && tmsg.arity() == 4;
        } catch (final Exception e) {
            return false;
        }
    }

    public OtpErlangPid getEventSender(final OtpErlangObject msg) {
        final OtpErlangTuple tmsg = (OtpErlangTuple) msg;
        return (OtpErlangPid) tmsg.elementAt(3);
    }

    public OtpErlangObject getEventData(final OtpErlangObject msg) {
        final OtpErlangTuple tmsg = (OtpErlangTuple) msg;
        return tmsg.elementAt(2);
    }

    public String getEventTopic(final OtpErlangObject msg) {
        final OtpErlangTuple tmsg = (OtpErlangTuple) msg;
        final Object el0 = tmsg.elementAt(1);
        final OtpErlangAtom a = (OtpErlangAtom) el0;
        return a.atomValue();
    }

}
