package org.erlide.launch.debug.events;

import org.erlide.launch.debug.model.ErlangDebugTarget;

import com.ericsson.otp.erlang.OtpErlangPid;

public class StartedEvent implements DebuggerEvent {

    private final OtpErlangPid pid;

    public StartedEvent(final OtpErlangPid pid) {
        this.pid = pid;
    }

    public OtpErlangPid getPid() {
        return pid;
    }

    @Override
    public void execute(final ErlangDebugTarget debugTarget) {
        debugTarget.started();
    }
}
