package org.erlide.runtime.events;

import org.erlide.util.ErlLogger;
import org.erlide.util.erlang.Bindings;
import org.erlide.util.erlang.ErlUtils;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.eventbus.Subscribe;

public class LogEventHandler extends ErlangEventHandler {

    public LogEventHandler(final String backendName) {
        super("log", backendName);
    }

    @Subscribe
    public void handleEvent(final ErlEvent event) {
        if (!event.getTopic().equals(getTopic())) {
            return;
        }
        final OtpErlangObject data = event.getEvent();
        try {
            final Bindings b = ErlUtils.match("{K:a,M}", data);
            final String kind = ((OtpErlangAtom) b.get("K")).atomValue();
            final OtpErlangObject amsg = b.get("M");
            ErlLogger.debug("%s: %s", kind, ErlUtils.asString(amsg));
        } catch (final Exception e) {
            ErlLogger.error("erroneous log msg: %s", data);
        }
    }
}
