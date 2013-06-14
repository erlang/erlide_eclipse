package org.erlide.backend.internal;

import org.erlide.runtime.api.ErlSystemStatus;
import org.erlide.runtime.api.IErlRuntime;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.IRuntimeStateListener;
import org.erlide.runtime.api.RuntimeData;
import org.erlide.runtime.rpc.IRpcCallback;
import org.erlide.runtime.rpc.IRpcFuture;
import org.erlide.runtime.rpc.IRpcResultCallback;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.runtime.rpc.RpcResult;
import org.erlide.runtime.shell.IBackendShell;
import org.erlide.util.ExtensionHelper;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;

public class NullErlRuntime implements IErlRuntime, IRpcSite {

    private void reportNoBackend() {
        final Runnable handler = (Runnable) ExtensionHelper
                .getParticipant("org.erlide.backend.no_runtime_handler");
        if (handler != null) {
            handler.run();
        }
    }

    @Override
    public RpcResult call_noexception(final String m, final String f,
            final String signature, final Object... a) {
        reportNoBackend();
        return null;
    }

    @Override
    public RpcResult call_noexception(final long timeout, final String m,
            final String f, final String signature, final Object... args) {
        reportNoBackend();
        return null;
    }

    @Override
    public IRpcFuture async_call(final String m, final String f,
            final String signature, final Object... args) throws RpcException {
        reportNoBackend();
        return null;
    }

    @Override
    public IRpcFuture async_call(final OtpErlangObject gleader, final String m,
            final String f, final String signature, final Object... args)
            throws RpcException {
        reportNoBackend();
        return null;
    }

    @Override
    public void async_call_cb(final IRpcCallback cb, final String m,
            final String f, final String signature, final Object... args)
            throws RpcException {
        reportNoBackend();

    }

    @Override
    public void async_call_cb(final IRpcCallback cb, final long timeout,
            final String m, final String f, final String signature,
            final Object... args) throws RpcException {
        reportNoBackend();

    }

    @Override
    public void async_call_cb(final IRpcCallback cb, final long timeout,
            final OtpErlangObject gleader, final String m, final String f,
            final String signature, final Object... args) throws RpcException {
        reportNoBackend();

    }

    @Override
    public void cast(final String m, final String f, final String signature,
            final Object... args) throws RpcException {
        reportNoBackend();

    }

    @Override
    public void cast(final OtpErlangObject gleader, final String m,
            final String f, final String signature, final Object... args)
            throws RpcException {
        reportNoBackend();

    }

    @Override
    public OtpErlangObject call(final String m, final String f,
            final String signature, final Object... a) throws RpcException {
        reportNoBackend();
        return null;
    }

    @Override
    public OtpErlangObject call(final long timeout, final String m,
            final String f, final String signature, final Object... a)
            throws RpcException {
        reportNoBackend();
        return null;
    }

    @Override
    public OtpErlangObject call(final long timeout,
            final OtpErlangObject gleader, final String m, final String f,
            final String signature, final Object... a) throws RpcException {
        reportNoBackend();
        return null;
    }

    @Override
    public void async_call_result(final IRpcResultCallback cb, final String m,
            final String f, final String signature, final Object... args)
            throws RpcException {
        reportNoBackend();

    }

    @Override
    public void send(final OtpErlangPid pid, final Object msg) {
        reportNoBackend();

    }

    @Override
    public void send(final String name, final Object msg) {
        reportNoBackend();

    }

    @Override
    public void send(final String fullNodeName, final String name,
            final Object msg) {
        reportNoBackend();

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
    public boolean isStopped() {
        reportNoBackend();
        return false;
    }

    @Override
    public String getNodeName() {
        reportNoBackend();
        return null;
    }

    @Override
    public Process start() {
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
    public OtpErlangPid getEventPid() {
        reportNoBackend();
        return null;
    }

    @Override
    public OtpMbox getEventMbox() {
        reportNoBackend();
        return null;
    }

    @Override
    public void restart() {
        reportNoBackend();

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
    public void tryConnect() {
    }

    @Override
    public OtpErlangObject getRpcResult(final OtpMbox mbox, final long timeout,
            final String env) throws RpcException {
        reportNoBackend();
        return null;
    }

    @Override
    public void registerEventHandler(final Object handler) {
    }
}
