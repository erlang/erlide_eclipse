package org.erlide.backend.debug.events;

import org.eclipse.debug.core.DebugEvent;
import org.erlide.backend.debug.model.ErlangDebugTarget;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;

public class NoInterpretEvent extends IntEvent {

    public NoInterpretEvent(final OtpErlangObject[] cmds) {
        super(cmds);
    }

    @Override
    public void execute(final ErlangDebugTarget debugTarget) {
        final OtpErlangAtom m = (OtpErlangAtom) cmds[1];
        debugTarget.getInterpretedModules().remove(m.atomValue());
        debugTarget.fireEvent(new DebugEvent(this, DebugEvent.MODEL_SPECIFIC,
                ErlangDebugTarget.INTERPRETED_MODULES_CHANGED));
    }

}
