package org.erlide.launch.debug.events;

import org.erlide.launch.debug.model.ErlangDebugTarget;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class MetaEvent implements DebuggerEvent {

    private final OtpErlangObject event;
    private final OtpErlangPid pid;

    public MetaEvent(final OtpErlangPid pid, final OtpErlangObject event) {
        this.pid = pid;
        this.event = event;
    }

    @Override
    public void execute(final ErlangDebugTarget debugTarget) {
        if (event instanceof OtpErlangTuple) {
            debugTarget.handleMetaEvent(pid, (OtpErlangTuple) event);
        } else {
            // TODO ??
        }
    }

}
