package org.erlide.jinterface.backend.events;

import org.erlide.jinterface.util.Bindings;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.jinterface.util.ErlUtils;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class LogEventHandler extends EventHandler {

    @Override
    protected void doHandleMsg(final OtpErlangObject msg) throws Exception {
        handleJavaLog(msg);
        handleErlangLog(msg);
    }

    private void handleErlangLog(final OtpErlangObject msg) {
        final OtpErlangObject log = getStandardEvent(msg, "erlang_log");
        if (log != null) {
            final OtpErlangTuple t = (OtpErlangTuple) log;
            final OtpErlangAtom module = (OtpErlangAtom) t.elementAt(0);
            final OtpErlangLong line = (OtpErlangLong) t.elementAt(1);
            final OtpErlangAtom level = (OtpErlangAtom) t.elementAt(2);
            final OtpErlangObject logEvent = t.elementAt(3);
            String ss = "";
            if (t.arity() == 5) {
                final OtpErlangTuple backtrace_0 = (OtpErlangTuple) t
                        .elementAt(4);
                final OtpErlangBinary backtrace = (OtpErlangBinary) backtrace_0
                        .elementAt(1);
                ss = new String(backtrace.binaryValue());
            }
            try {
                ErlLogger.getInstance().erlangLog(module.atomValue() + ".erl",
                        line.uIntValue(), level.atomValue().toUpperCase(),
                        "%s %s", logEvent.toString(), ss);
            } catch (final Exception e) {
                ErlLogger.warn(e);
            }
        }
    }

    private void handleJavaLog(final OtpErlangObject msg) {
        final OtpErlangObject log = getStandardEvent(msg, "log");
        if (log != null) {
            try {
                final Bindings b = ErlUtils.match("{K:a,M}", log);
                final String kind = ((OtpErlangAtom) b.get("K")).atomValue();
                final OtpErlangObject amsg = b.get("M");
                ErlLogger.debug("%s: %s", kind, ErlUtils.asString(amsg));
            } catch (final Exception e) {
                ErlLogger.error("erroneous log msg: %s", log);
            }
        }
    }

}
