package org.erlide.debug.ui.tracing;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.debug.core.model.IStackFrame;
import org.eclipse.debug.core.model.IThread;
import org.erlide.launch.debug.model.ErlangProcess;
import org.erlide.launch.debug.model.ErlangStackFrame;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class DebugTraceEvent {

    private final OtpErlangPid pid;
    private final OtpErlangTuple event;

    public DebugTraceEvent(final OtpErlangPid pid, final OtpErlangTuple event) {
        super();
        this.pid = pid;
        this.event = event;
    }

    public OtpErlangPid getPid() {
        return pid;
    }

    // event: {what, {ieval, extra}, stack}
    // ieval (record):
    // level = 1, % Current call level
    // line = -1, % Current source code line (of module)
    // module, % MFA which called the currently
    // function, % interpreted function
    // arguments, %
    // last_call = false % True if current expression is the VERY last to be
    // evaluated (ie at all, not only in a clause)
    // extra:
    // {Val} % on return
    // {_, _, BindingsImpl} % otherwise

    public OtpErlangTuple getTuple() {
        return event;
    }

    public List<IStackFrame> getStackFrames(final IDebugTarget target,
            final IThread process) {
        // XXX JC copy paste
        final OtpErlangTuple tuple = getTuple();
        final OtpErlangList erlStackFrames = (OtpErlangList) tuple.elementAt(2);
        final OtpErlangTuple t2 = (OtpErlangTuple) tuple.elementAt(1);
        final OtpErlangTuple ieval = (OtpErlangTuple) t2.elementAt(0);
        OtpErlangAtom m = (OtpErlangAtom) ieval.elementAt(3);
        OtpErlangList bindings = (OtpErlangList) t2.elementAt(t2.arity() - 1);
        OtpErlangLong l = (OtpErlangLong) ieval.elementAt(1);
        final List<IStackFrame> stackFrames = new ArrayList<IStackFrame>(
                erlStackFrames.arity() + 1);
        for (final OtpErlangObject o : erlStackFrames) {
            final OtpErlangTuple t = (OtpErlangTuple) o;
            final OtpErlangTuple ml = (OtpErlangTuple) t.elementAt(1);
            final OtpErlangObject ml0 = ml.elementAt(0);
            int stackFrameNo;
            final OtpErlangLong n = (OtpErlangLong) t.elementAt(3);
            try {
                stackFrameNo = n.intValue();
            } catch (final OtpErlangRangeException e) {
                stackFrameNo = -1;
            }
            final String module = m.atomValue();
            int line;
            try {
                line = l.intValue();
            } catch (final OtpErlangRangeException e) {
                line = -1;
            }
            final IStackFrame sf = new ErlangStackFrame(module,
                    (ErlangProcess) process, target, line, null, bindings,
                    stackFrameNo);
            stackFrames.add(sf);
            bindings = (OtpErlangList) t.elementAt(2);
            m = (OtpErlangAtom) ml0;
            l = (OtpErlangLong) ml.elementAt(1);
        }
        return stackFrames;
    }
}
