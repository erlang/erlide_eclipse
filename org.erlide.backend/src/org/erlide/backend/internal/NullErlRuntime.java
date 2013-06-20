package org.erlide.backend.internal;

import org.erlide.runtime.api.ErlSystemStatus;
import org.erlide.runtime.api.IErlRuntime;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.IRuntimeStateListener;
import org.erlide.runtime.api.RuntimeData;
import org.erlide.util.ExtensionHelper;

import com.ericsson.otp.erlang.OtpMbox;
import com.google.common.util.concurrent.Service.State;

public class NullErlRuntime implements IErlRuntime {

    private void reportNoBackend() {
        final Runnable handler = (Runnable) ExtensionHelper
                .getParticipant("org.erlide.backend.no_runtime_handler");
        if (handler != null) {
            handler.run();
        }
    }

    @Override
    public RuntimeData getRuntimeData() {
        reportNoBackend();
        return null;
    }

    @Override
    public boolean isRunning() {
        reportNoBackend();
        return false;
    }

    @Override
    public String getNodeName() {
        reportNoBackend();
        return null;
    }

    @Override
    public OtpMbox createMbox(final String string) {
        reportNoBackend();
        return null;
    }

    @Override
    public OtpMbox createMbox() {
        reportNoBackend();
        return null;
    }

    @Override
    public OtpMbox getEventMbox() {
        reportNoBackend();
        return null;
    }

    @Override
    public void addListener(final IRuntimeStateListener listener) {
        reportNoBackend();
    }

    @Override
    public ErlSystemStatus getSystemStatus() {
        reportNoBackend();
        return null;
    }

    @Override
    public void setSystemStatus(final ErlSystemStatus msg) {
        reportNoBackend();
    }

    @Override
    public void dispose() {
    }

    @Override
    public IRpcSite getRpcSite() {
        reportNoBackend();
        return null;
    }

    @Override
    public void registerEventListener(final Object handler) {
        reportNoBackend();
    }

    @Override
    public Process getProcess() {
        reportNoBackend();
        return null;
    }

    @Override
    public State startAndWait() {
        reportNoBackend();
        return State.RUNNING;
    }
}
