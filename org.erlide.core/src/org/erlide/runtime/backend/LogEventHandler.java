package org.erlide.runtime.backend;

import org.erlide.jinterface.Bindings;
import org.erlide.jinterface.ErlUtils;
import org.erlide.runtime.ErlLogger;
import org.erlide.runtime.backend.events.EventHandler;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class LogEventHandler extends EventHandler {

	@Override
	protected void doHandleMsg(OtpErlangObject msg) throws Exception {
		OtpErlangObject log = getStandardEvent(msg, "log");
		if (log != null) {
			try {
				Bindings b = ErlUtils.match("{K:a,M}", log);
				String kind = ((OtpErlangAtom) b.get("K")).atomValue();
				OtpErlangObject amsg = b.get("M");
				ErlLogger.debug("%s: %s", kind, amsg);
			} catch (Exception e) {
				ErlLogger.error(e);
			}
		}

		log = getStandardEvent(msg, "erlang_log");
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
				ErlLogger.erlangLog(module.atomValue() + ".erl", line
						.uIntValue(), level.atomValue().toUpperCase(), "%s %s",
						logEvent.toString(), ss);
			} catch (final Exception e) {
				ErlLogger.warn(e);
			}
		}
	}

}
