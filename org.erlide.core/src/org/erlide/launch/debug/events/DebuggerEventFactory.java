package org.erlide.launch.debug.events;

import org.erlide.jinterface.Bindings;
import org.erlide.jinterface.ErlLogger;
import org.erlide.utils.ErlUtils;
import org.erlide.utils.TermParserException;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangObject;

public class DebuggerEventFactory {

    public static DebuggerEvent parse(final OtpErlangObject message) {
        // TODO More events from erlide_dbg_mon...
        try {
            Bindings b = ErlUtils.match("{started, Pid:p}", message);
            if (b != null) {
                return buildStartedEvent(b);
            }
            b = ErlUtils.match("{terminated, Pid:p}", message);
            if (b != null) {
                return buildTerminatedEvent(b);
            }
            b = ErlUtils.match("{int, Cmd}", message);
            if (b != null) {
                return buildIntEvent(b);
            }
            b = ErlUtils.match("{attached, Pid:p}", message);
            if (b != null) {
                return buildAttachedEvent(b);
            }
            b = ErlUtils.match("{Other:a, Cmd}", message);
            if (b != null) {
                return buildUnknownEvent(message);
            }
            b = ErlUtils.match("{Meta:p, Event}", message);
            if (b != null) {
                return buildMetaEvent(b);
            }
        } catch (final TermParserException e) {
            ErlLogger.error(e);
        } catch (final OtpErlangException e) {
            ErlLogger.error(e);
        }
        return new UnknownEvent(message);
    }

    private static DebuggerEvent buildMetaEvent(final Bindings b)
            throws OtpErlangException {
        return new MetaEvent(b.getPid("Meta"), b.get("Event"));
    }

    private static DebuggerEvent buildUnknownEvent(final OtpErlangObject message) {
        return new UnknownEvent(message);
    }

    private static DebuggerEvent buildAttachedEvent(final Bindings b)
            throws OtpErlangException {
        return new AttachedEvent(b.getPid("Pid"));
    }

    private static DebuggerEvent buildIntEvent(final Bindings b) {
        return new IntEvent(b.get("Cmd"));
    }

    private static DebuggerEvent buildTerminatedEvent(final Bindings b)
            throws OtpErlangException {
        return new TerminatedEvent(b.getPid("Pid"));
    }

    private static DebuggerEvent buildStartedEvent(final Bindings b)
            throws OtpErlangException {
        return new StartedEvent(b.getPid("Pid"));
    }

}
