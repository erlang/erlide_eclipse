package org.erlide.backend.debug.events;

import org.eclipse.debug.core.DebugEvent;
import org.erlide.backend.debug.model.ErlangDebugTarget;
import org.erlide.backend.debug.model.ErlangProcess;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ExitAtEvent extends MetaEvent {

    private final String module;
    private final int line;
    @SuppressWarnings("unused")
    private final OtpErlangPid metaPid;
    private final OtpErlangPid pid;
    private final OtpErlangList bindings;
    private final OtpErlangList stackFrames;

    public ExitAtEvent(final OtpErlangPid metaPid,
            final OtpErlangObject otpErlangObject2,
            final OtpErlangObject otpErlangObject3,
            final OtpErlangObject otpErlangObject, final OtpErlangPid pid2) {
        this(metaPid, otpErlangObject2, otpErlangObject3, otpErlangObject, pid2, null,
                null);
    }

    public ExitAtEvent(final OtpErlangPid metaPid, final OtpErlangObject pos,
            final OtpErlangObject reason, final OtpErlangObject otpErlangObject,
            final OtpErlangPid pid2, final OtpErlangList stackFrames,
            final OtpErlangList bindings) {
        super(metaPid, null);
        if (pos instanceof OtpErlangTuple) {
            final OtpErlangTuple t = (OtpErlangTuple) pos;
            module = ((OtpErlangAtom) t.elementAt(0)).atomValue();
            line = parseLine((OtpErlangLong) t.elementAt(1));
        } else {
            module = null;
            line = -1;
        }
        this.metaPid = metaPid;
        pid = pid2;
        this.stackFrames = stackFrames;
        this.bindings = bindings;
    }

    private int parseLine(final OtpErlangLong elementAt) {
        try {
            return elementAt.intValue();
        } catch (final OtpErlangRangeException e) {
            return -1;
        }
    }

    @Override
    public void execute(final ErlangDebugTarget debugTarget) {
        final ErlangProcess erlangProcess = debugTarget
                .getOrCreateErlangProcess(getPid(debugTarget));

        if (module == null || line == -1) {
            erlangProcess.removeStackFrames();
            erlangProcess.fireSuspendEvent(DebugEvent.TERMINATE);
        }
        if (stackFrames != null) {
            erlangProcess.setStackFrames(module, line, stackFrames, bindings);
        }
        erlangProcess.fireSuspendEvent(DebugEvent.TERMINATE);
    }

    @Override
    public OtpErlangPid getPid(final ErlangDebugTarget debugTarget) {
        return pid;
    }

    public String getModule() {
        return module;
    }

    public int getLine() {
        return line;
    }
}
