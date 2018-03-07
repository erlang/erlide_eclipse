package org.erlide.runtime.events;

import org.erlide.util.ErlLogger;
import org.erlide.util.erlang.OtpBindings;
import org.erlide.util.erlang.OtpErlang;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.eventbus.Subscribe;

public class LogEventHandler extends ErlangEventHandler {

    public LogEventHandler() {
        super("log");
    }

    @Subscribe
    public void handleEvent(final ErlEvent event) {
        if (!event.getTopic().equals(getTopic())) {
            return;
        }
        final OtpErlangObject data = event.getEvent();
        try {
            final OtpBindings b = OtpErlang.match("{K:a,M}", data);
            final String kind = ((OtpErlangAtom) b.get("K")).atomValue();
            final OtpErlangObject amsg = b.get("M");
            ErlLogger.debug("%s: %s", kind, OtpErlang.asString(amsg));
        } catch (final Exception e) {
            ErlLogger.error("erroneous log msg: %s", data);
        }
    }
}
