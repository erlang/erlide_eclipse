package org.erlide.backend.debug.model;

import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IDebugTarget;

import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class TraceChangedEventData {
    public static final int ADDED = 1;
    private final int what;
    private final ILaunch launch;
    private final IDebugTarget node;
    private final OtpErlangPid pid;
    private final OtpErlangTuple[] events;

    public ILaunch getLaunch() {
        return launch;
    }

    public IDebugTarget getNode() {
        return node;
    }

    public TraceChangedEventData(final int what, final ILaunch launch,
            final IDebugTarget node, final OtpErlangPid pid, final OtpErlangTuple[] events) {
        super();
        this.what = what;
        this.launch = launch;
        this.node = node;
        this.pid = pid;
        this.events = events;
    }

    public int getWhat() {
        return what;
    }

    public OtpErlangTuple[] getEvents() {
        return events;
    }

    public OtpErlangPid getPid() {
        return pid;
    }

}
