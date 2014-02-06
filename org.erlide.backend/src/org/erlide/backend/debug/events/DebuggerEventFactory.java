package org.erlide.backend.debug.events;

import org.erlide.util.ErlLogger;
import org.erlide.util.erlang.Bindings;
import org.erlide.util.erlang.ErlUtils;
import org.erlide.util.erlang.TermParserException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public class DebuggerEventFactory {

    public static DebuggerEvent parse(final OtpErlangObject message) {
        // TODO More events from dbg_mon...
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
        return parseMeta(b.getPid("Meta"), b.get("Event"));
    }

    private static MetaEvent parseMeta(final OtpErlangPid pid, final OtpErlangObject event) {
        try {
            Bindings b = ErlUtils.match("{break_at, Mod:a, Line:i, Crt}", event);
            if (b != null) {
                return new BreakAtEvent(pid, b.getAtom("Mod"), b.getInt("Line"),
                        b.get("Crt"));
            }
            b = ErlUtils.match("{exit_at, Pos, Reason, Le, OrigPid:p}", event);
            if (b != null) {
                return new ExitAtEvent(pid, b.get("Pos"), b.get("Reason"), b.get("Le"),
                        b.getPid("OrigPid"));
            }
            b = ErlUtils.match("{exit_at, Pos, Reason, Le, OrigPid:p, Stack:l, Binds:l}",
                    event);
            if (b != null) {
                return new ExitAtEvent(pid, b.get("Pos"), b.get("Reason"), b.get("Le"),
                        b.getPid("OrigPid"), (OtpErlangList) b.get("Stack"),
                        (OtpErlangList) b.get("Binds"));
            }
            b = ErlUtils.match("{wait_at, Mod:a, Line:i, Crt}", event);
            if (b != null) {
                return new WaitAtEvent(pid, b.getAtom("Mod"), b.getInt("Line"),
                        b.get("Crt"));
            }
        } catch (final Exception e) {
        }
        // this is a default event that does nothing
        return new MetaEvent(pid, event);
    }

    private static DebuggerEvent buildUnknownEvent(final OtpErlangObject message) {
        return new UnknownEvent(message);
    }

    private static DebuggerEvent buildAttachedEvent(final Bindings b)
            throws OtpErlangException {
        return new AttachedEvent(b.getPid("Pid"));
    }

    private static DebuggerEvent buildIntEvent(final Bindings b) {
        try {
            final OtpErlangObject[] cmds = b.getTuple("Cmd");
            final String cmd = ((OtpErlangAtom) cmds[0]).atomValue();
            if (cmd.equals("new_break")) {
                return new NewBreakEvent(cmds);
            } else if (cmd.equals("new_status")) {
                return new NewStatusEvent(cmds);
            } else if (cmd.equals("new_process")) {
                return new NewProcessEvent(cmds);
            } else if (cmd.equals("interpret")) {
                return new InterpretEvent(cmds);
            } else if (cmd.equals("no_interpret")) {
                return new NoInterpretEvent(cmds);
            } else {
                return new IntEvent(cmds);
            }
        } catch (final Exception e) {
            return new IntEvent(new OtpErlangObject[] { new OtpErlangAtom("nop") });
        }
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
