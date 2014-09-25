package org.erlide.backend.debug.events;

import org.erlide.backend.debug.ErlideDebug;
import org.erlide.backend.debug.model.ErlangDebugTarget;

import com.ericsson.otp.erlang.OtpErlangPid;

public class AttachedEvent implements DebuggerEvent {

    private final OtpErlangPid pid;

    public AttachedEvent(final OtpErlangPid pid) {
        this.pid = pid;
    }

    @Override
    public void execute(final ErlangDebugTarget debugTarget) {
        if (debugTarget.getMetaFromPid(pid) == null) {
            final OtpErlangPid self = debugTarget.getEventMBox();
            final OtpErlangPid metaPid = ErlideDebug.attached(debugTarget.getBackend()
                    .getOtpRpc(), pid, self);
            // ErlLogger.debug("attached: " + pid + ",  meta: " + metaPid);
            if (metaPid != null) {
                debugTarget.putMetaPid(metaPid, pid);
            }
            // ErlideDebug.tracing(runtime, true, metaPid);
        }
    }

}
