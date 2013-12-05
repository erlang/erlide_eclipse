package org.erlide.backend.debug.events;

import org.eclipse.debug.core.DebugEvent;
import org.erlide.backend.debug.model.ErlangDebugTarget;
import org.erlide.backend.debug.model.ErlangProcess;
import org.erlide.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public class BreakAtEvent extends MetaEvent {

    private final String module;
    private final int line;

    public BreakAtEvent(final OtpErlangPid pid, final String module, final int line,
            final OtpErlangObject le) {
        super(pid, null);
        this.module = module;
        this.line = line;
    }

    @Override
    public void execute(final ErlangDebugTarget debugTarget) {
        if (module == null || line == -1) {
            ErlLogger.debug("can't do anything with no module/line defined");
            return;
        }

        final ErlangProcess erlangProcess = debugTarget
                .getOrCreateErlangProcess(getPid(debugTarget));
        // FIXME can't get stack in wait...
        // should be possible according to dbg_ui_trace_win....
        erlangProcess.getStackAndBindings(module, line);
        if (erlangProcess.isStepping()) {
            erlangProcess.fireSuspendEvent(DebugEvent.STEP_END);
        } else {
            erlangProcess.fireSuspendEvent(DebugEvent.BREAKPOINT);
        }
        erlangProcess.setNotStepping();
    }

    public String getModule() {
        return module;
    }

    public int getLine() {
        return line;
    }

}
