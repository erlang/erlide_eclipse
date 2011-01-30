package org.erlide.jinterface.backend.events;

import java.util.Collection;

import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public abstract class EventHandler {

    public final void handleMsgs(final Collection<OtpErlangObject> msgs) {
        for (final OtpErlangObject msg : msgs) {
            handleMsg(msg);
        }
    }

    public void handleMsg(final OtpErlangObject msg) {
        if (msg == null) {
            return;
        }
        try {
            doHandleMsg(msg);
        } catch (final Exception e) {
            ErlLogger.debug(e);
            // ignore unrecognized messages
        }
    }

    protected abstract void doHandleMsg(OtpErlangObject msg) throws Exception;

    public static OtpErlangObject getStandardEvent(final OtpErlangObject msg,
            final OtpErlangAtom id) {
        return getStandardEvent(msg, id.atomValue());
    }

    public static OtpErlangObject getStandardEvent(final OtpErlangObject msg,
            final String id) {
        try {
            if (isEventMessage(msg)) {
                final String topic = getEventTopic(msg);
                if (id.equals(topic)) {
                    return getEventData(msg);
                }

            }
        } catch (final Exception e) {
        }
        return null;
    }

    public static OtpErlangObject getEventData(final OtpErlangObject msg) {
        final OtpErlangTuple tmsg = (OtpErlangTuple) msg;
        return tmsg.elementAt(2);
    }

    public static String getEventTopic(final OtpErlangObject msg) {
        final OtpErlangTuple tmsg = (OtpErlangTuple) msg;
        final Object el0 = tmsg.elementAt(1);
        final OtpErlangAtom a = (OtpErlangAtom) el0;
        return a.atomValue();
    }

    private static boolean isEventMessage(final OtpErlangObject msg) {
        final OtpErlangTuple tmsg = (OtpErlangTuple) msg;
        final OtpErlangObject el0 = tmsg.elementAt(0);
        return ((OtpErlangAtom) el0).atomValue().equals("event");
    }

}
