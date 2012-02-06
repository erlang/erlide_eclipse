package org.erlide.launch.debug.model;

import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.IDebugTarget;
import org.erlide.core.model.util.ErlangFunction;

import com.ericsson.otp.erlang.OtpErlangList;

public class ErlangUninterpretedStackFrame extends ErlangStackFrame {

    private final ErlangFunction function;

    public ErlangUninterpretedStackFrame(final String module,
            final ErlangFunction function, final ErlangProcess erlangProcess,
            final IDebugTarget target) {
        super(module, erlangProcess, target, -1, function, new OtpErlangList(),
                -1);
        this.function = function;
    }

    @Override
    public boolean canStepInto() {
        return false;
    }

    @Override
    public boolean canStepOver() {
        return false;
    }

    @Override
    public boolean canStepReturn() {
        return false;
    }

    @Override
    public boolean isStepping() {
        return false;
    }

    @Override
    public void stepInto() throws DebugException {
    }

    @Override
    public void stepOver() throws DebugException {
    }

    @Override
    public void stepReturn() throws DebugException {
    }

    @Override
    public boolean canResume() {
        return false;
    }

    @Override
    public boolean canSuspend() {
        return false;
    }

    @Override
    public boolean isSuspended() {
        return false;
    }

    @Override
    public void resume() throws DebugException {
    }

    @Override
    public void suspend() throws DebugException {
    }

    @Override
    public boolean canTerminate() {
        return false;
    }

    @Override
    public boolean isTerminated() {
        return false;
    }

    @Override
    public void terminate() throws DebugException {
    }

    public ErlangFunction getFunction() {
        return function;
    }

}
