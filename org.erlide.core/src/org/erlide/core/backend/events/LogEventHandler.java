package org.erlide.core.backend.events;

import org.erlide.core.backend.IBackend;
import org.erlide.jinterface.Bindings;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.util.ErlUtils;
import org.osgi.service.event.Event;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;

public class LogEventHandler extends ErlangEventHandler {

    public LogEventHandler(final IBackend backend) {
        super("log", backend);
    }

    public void handleEvent(final Event event) {
        final String data = (String) event.getProperty("DATA");
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
