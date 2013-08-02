package org.erlide.runtime.internal;

import org.erlide.runtime.api.ErlSystemStatus;
import org.erlide.runtime.api.IErlRuntime;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.IShutdownCallback;
import org.erlide.runtime.api.NoRuntimeEvent;
import org.erlide.runtime.api.RuntimeData;
import org.erlide.util.ErlideEventBus;

import com.ericsson.otp.erlang.OtpMbox;
import com.google.common.util.concurrent.Service.State;

public class NullErlRuntime implements IErlRuntime {

    private void reportNoBackend() {
        ErlideEventBus.post(new NoRuntimeEvent());
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
    public void addShutdownCallback(final IShutdownCallback callback) {
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
