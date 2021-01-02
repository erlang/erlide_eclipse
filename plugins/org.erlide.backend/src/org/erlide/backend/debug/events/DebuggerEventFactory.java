package org.erlide.backend.debug.events;

import org.erlide.util.ErlLogger;
import org.erlide.util.erlang.OtpBindings;
import org.erlide.util.erlang.OtpErlang;
import org.erlide.util.erlang.OtpParserException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public class DebuggerEventFactory {

    public static DebuggerEvent parse(final OtpErlangObject message) {
        ErlLogger.info("debugger: %s", message);
        // TODO More events from dbg_mon...
        try {
            OtpBindings b = OtpErlang.match("{started, Pid:p}", message);
            if (b != null) {
                return DebuggerEventFactory.buildStartedEvent(b);
            }
            b = OtpErlang.match("{terminated, Pid:p}", message);
            if (b != null) {
                return DebuggerEventFactory.buildTerminatedEvent(b);
            }
            b = OtpErlang.match("{int, Cmd}", message);
            if (b != null) {
                return DebuggerEventFactory.buildIntEvent(b);
            }
            b = OtpErlang.match("{attached, Pid:p}", message);
            if (b != null) {
                return DebuggerEventFactory.buildAttachedEvent(b);
            }
            b = OtpErlang.match("{Other:a, Cmd}", message);
            if (b != null) {
                return DebuggerEventFactory.buildUnknownEvent(message);
            }
            b = OtpErlang.match("{Meta:p, Event}", message);
            if (b != null) {
                return DebuggerEventFactory.buildMetaEvent(b);
            }
        } catch (final OtpParserException e) {
            ErlLogger.error(e);
        } catch (final OtpErlangException e) {
            ErlLogger.error(e);
        }
        return new UnknownEvent(message);
    }

    private static DebuggerEvent buildMetaEvent(final OtpBindings b)
            throws OtpErlangException {
        return DebuggerEventFactory.parseMeta(b.getPid("Meta"), b.get("Event"));
    }

    private static MetaEvent parseMeta(final OtpErlangPid pid,
            final OtpErlangObject event) {
        try {
            OtpBindings b = OtpErlang.match("{break_at, Mod:a, Line:i, Crt}", event);
            if (b != null) {
                return new BreakAtEvent(pid, b.getAtom("Mod"), b.getInt("Line"),
                        b.get("Crt"));
            }
            b = OtpErlang.match("{exit_at, Pos, Reason, Le, OrigPid:p}", event);
            if (b != null) {
                return new ExitAtEvent(pid, b.get("Pos"), b.get("Reason"), b.get("Le"),
                        b.getPid("OrigPid"));
            }
            b = OtpErlang.match("{exit_at, Pos, Reason, Le, OrigPid:p, Stack:l, Binds:l}",
                    event);
            if (b != null) {
                return new ExitAtEvent(pid, b.get("Pos"), b.get("Reason"), b.get("Le"),
                        b.getPid("OrigPid"), (OtpErlangList) b.get("Stack"),
                        (OtpErlangList) b.get("Binds"));
            }
            b = OtpErlang.match("{wait_at, Mod:a, Line:i, Crt}", event);
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

    private static DebuggerEvent buildAttachedEvent(final OtpBindings b)
            throws OtpErlangException {
        return new AttachedEvent(b.getPid("Pid"));
    }

    private static DebuggerEvent buildIntEvent(final OtpBindings b) {
        try {
            final OtpErlangObject[] cmds = b.getTuple("Cmd");
            final String cmd = ((OtpErlangAtom) cmds[0]).atomValue();
            if ("new_break".equals(cmd)) {
                return new NewBreakEvent(cmds);
            } else if ("new_status".equals(cmd)) {
                return new NewStatusEvent(cmds);
            } else if ("new_process".equals(cmd)) {
                return new NewProcessEvent(cmds);
            } else if ("interpret".equals(cmd)) {
                return new InterpretEvent(cmds);
            } else if ("no_interpret".equals(cmd)) {
                return new NoInterpretEvent(cmds);
            } else {
                return new IntEvent(cmds);
            }
        } catch (final Exception e) {
            return new IntEvent(new OtpErlangObject[] {
                    new OtpErlangAtom("nop")
            });
        }
    }

    private static DebuggerEvent buildTerminatedEvent(final OtpBindings b)
            throws OtpErlangException {
        return new TerminatedEvent(b.getPid("Pid"));
    }

    private static DebuggerEvent buildStartedEvent(final OtpBindings b)
            throws OtpErlangException {
        return new StartedEvent(b.getPid("Pid"));
    }

}
