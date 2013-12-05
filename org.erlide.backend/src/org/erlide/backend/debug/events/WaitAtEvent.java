package org.erlide.backend.debug.events;

import org.erlide.backend.debug.model.ErlangDebugTarget;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public class WaitAtEvent extends MetaEvent {
    private final String module;
    private final int line;
    @SuppressWarnings("unused")
    private final OtpErlangPid metaPid;

    public WaitAtEvent(final OtpErlangPid metaPid, final String module, final int line,
            final OtpErlangObject otpErlangObject) {
        super(metaPid, null);
        this.module = module;
        this.line = line;
        this.metaPid = metaPid;
    }

    @Override
    public void execute(final ErlangDebugTarget debugTarget) {
        // do nothing?
    }

    public String getModule() {
        return module;
    }

    public int getLine() {
        return line;
    }
}
