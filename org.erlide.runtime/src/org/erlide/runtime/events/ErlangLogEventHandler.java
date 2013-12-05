package org.erlide.runtime.events;

import org.erlide.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.eventbus.Subscribe;

public class ErlangLogEventHandler extends ErlangEventHandler {

    public ErlangLogEventHandler(final String backendName) {
        super("erlang_log", backendName);
    }

    @Subscribe
    public void handleEvent(final ErlEvent event) {
        if (!event.getTopic().equals(getTopic())) {
            return;
        }
        final OtpErlangTuple t = (OtpErlangTuple) event.getEvent();
        final OtpErlangAtom module = (OtpErlangAtom) t.elementAt(0);
        final OtpErlangLong line = (OtpErlangLong) t.elementAt(1);
        final OtpErlangAtom level = (OtpErlangAtom) t.elementAt(2);
        final OtpErlangObject logEvent = t.elementAt(3);
        String ss = "";
        if (t.arity() == 5) {
            final OtpErlangTuple backtrace_0 = (OtpErlangTuple) t.elementAt(4);
            final OtpErlangBinary backtrace = (OtpErlangBinary) backtrace_0.elementAt(1);
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
