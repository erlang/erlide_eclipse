package org.erlide.backend.events;

import org.erlide.backend.IBackend;
import org.erlide.jinterface.ErlLogger;
import org.osgi.service.event.Event;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlangLogEventHandler extends ErlangEventHandler {

    public ErlangLogEventHandler(final IBackend backend) {
        super("erlang_log", backend);
    }

    @Override
    public void handleEvent(final Event event) {
        final OtpErlangTuple t = (OtpErlangTuple) event.getProperty("DATA");
        final OtpErlangAtom module = (OtpErlangAtom) t.elementAt(0);
        final OtpErlangLong line = (OtpErlangLong) t.elementAt(1);
        final OtpErlangAtom level = (OtpErlangAtom) t.elementAt(2);
        final OtpErlangObject logEvent = t.elementAt(3);
        String ss = "";
        if (t.arity() == 5) {
            final OtpErlangTuple backtrace_0 = (OtpErlangTuple) t.elementAt(4);
            final OtpErlangBinary backtrace = (OtpErlangBinary) backtrace_0
                    .elementAt(1);
            ss = new String(backtrace.binaryValue());
        }
        try {
            ErlLogger.getInstance().erlangLog(module.atomValue() + ".erl",
                    line.uIntValue(), level.atomValue().toUpperCase(), "%s %s",
                    logEvent.toString(), ss);
        } catch (final Exception e) {
            ErlLogger.warn(e);
        }
    }
}
