package org.erlide.runtime.internal;

import org.erlide.runtime.api.IErlRuntime;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.IShutdownCallback;
import org.erlide.runtime.events.NoRuntimeEvent;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.ErlideEventBus;

import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;

public class NullErlRuntime implements IErlRuntime {

    private void reportNoBackend() {
        ErlideEventBus.post(new NoRuntimeEvent());
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
    public OtpErlangPid getEventPid() {
        reportNoBackend();
        return null;
    }

    @Override
    public void setShutdownCallback(final IShutdownCallback callback) {
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
    public void startAndWait() {
        reportNoBackend();
    }

    @Override
    public RuntimeVersion getVersion() {
        reportNoBackend();
        return RuntimeVersion.NO_VERSION;
    }

    @Override
    public String getOtpHome() {
        reportNoBackend();
        return null;
    }
}
