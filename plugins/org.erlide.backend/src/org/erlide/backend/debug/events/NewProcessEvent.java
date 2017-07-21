package org.erlide.backend.debug.events;

import org.erlide.backend.debug.model.ErlangDebugTarget;
import org.erlide.backend.debug.model.ErlangProcess;
import org.erlide.util.ErlangFunctionCall;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class NewProcessEvent extends IntEvent {

    public NewProcessEvent(final OtpErlangObject[] cmds) {
        super(cmds);
    }

    @Override
    public void execute(final ErlangDebugTarget debugTarget) {
        final OtpErlangTuple t = (OtpErlangTuple) cmds[1];
        final OtpErlangPid pid = (OtpErlangPid) t.elementAt(0);
        final ErlangProcess erlangProcess = debugTarget.getOrCreateErlangProcess(pid);
        final OtpErlangAtom statusA = (OtpErlangAtom) t.elementAt(2);
        final String status = statusA.atomValue();
        erlangProcess.setStatus(status);
        final OtpErlangTuple initialCall = (OtpErlangTuple) t.elementAt(1);
        erlangProcess.setInitialCall(new ErlangFunctionCall(initialCall));
        erlangProcess.fireCreationEvent();
    }

}
