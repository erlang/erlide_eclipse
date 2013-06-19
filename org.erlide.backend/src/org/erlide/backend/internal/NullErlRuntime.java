package org.erlide.backend.internal;

import org.erlide.runtime.api.ErlSystemStatus;
import org.erlide.runtime.api.IErlRuntime;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.IRuntimeStateListener;
import org.erlide.runtime.api.RuntimeData;
import org.erlide.runtime.shell.IBackendShell;
import org.erlide.util.ExtensionHelper;

import com.ericsson.otp.erlang.OtpMbox;

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
    public boolean isAvailable() {
        reportNoBackend();
        return false;
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
    public void stop() {
        reportNoBackend();

    }

    @Override
    public void connect() {
        reportNoBackend();

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
    public IBackendShell getShell(final String id) {
        reportNoBackend();
        return null;
    }

    @Override
    public ErlSystemStatus getSystemStatus() {
        return null;
    }

    @Override
    public void setSystemStatus(final ErlSystemStatus msg) {
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
    public void registerEventHandler(final Object handler) {
    }

    @Override
    public Process getProcess() {
        return null;
    }
}
