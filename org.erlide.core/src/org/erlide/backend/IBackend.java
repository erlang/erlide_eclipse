package org.erlide.backend;

import java.io.IOException;

import org.eclipse.core.resources.IProject;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.erlide.backend.console.IBackendShell;
import org.erlide.backend.runtimeinfo.RuntimeInfo;
import org.erlide.jinterface.rpc.IRpcCallback;
import org.erlide.jinterface.rpc.IRpcFuture;
import org.erlide.jinterface.rpc.IRpcResultCallback;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.jinterface.rpc.RpcResult;
import org.erlide.utils.IDisposable;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;

public interface IBackend extends IDisposable {

    OtpErlangObject receiveEvent(final long timeout) throws OtpErlangExit,
            OtpErlangDecodeException;

    void connect();

    String getErlangVersion();

    OtpErlangPid getEventPid();

    RuntimeInfo getRuntimeInfo();

    String getName();

    String getFullNodeName();

    boolean isStopped();

    void stop();

    OtpMbox createMbox();

    OtpMbox createMbox(final String name);

    void registerCodeBundle(final ICodeBundle bundle);

    void unregisterCodeBundle(final Bundle b);

    ILaunch getLaunch();

    IBackendShell getShell(final String id);

    boolean isDistributed();

    void input(final String s) throws IOException;

    void addProjectPath(final IProject project);

    void removeProjectPath(final IProject project);

    boolean isManaged();

    boolean doLoadOnAllNodes();

    IStreamsProxy getStreamsProxy();

    boolean hasConsole();

    BackendData getData();

    void initialize();

    void installDeferredBreakpoints();

    void interpret(IProject project, String moduleName, boolean distributed,
            boolean interpret);

    void setMonitoring(boolean on);

    void setMonitoringInterval(int monitoringInterval);

    /**
     * typed RPC
     * 
     */
    RpcResult call_noexception(final String m, final String f,
            final String signature, final Object... a);

    /**
     * typed RPC with timeout
     * 
     * @throws ConversionException
     */
    RpcResult call_noexception(final int timeout, final String m,
            final String f, final String signature, final Object... args);

    IRpcFuture async_call(final String m, final String f,
            final String signature, final Object... args) throws RpcException;

    void async_call_cb(final IRpcCallback cb, final String m, final String f,
            final String signature, final Object... args) throws RpcException;

    void cast(final String m, final String f, final String signature,
            final Object... args) throws RpcException;

    OtpErlangObject call(final String m, final String f,
            final String signature, final Object... a) throws RpcException;

    OtpErlangObject call(final int timeout, final String m, final String f,
            final String signature, final Object... a) throws RpcException;

    OtpErlangObject call(final int timeout, final OtpErlangObject gleader,
            final String m, final String f, final String signature,
            final Object... a) throws RpcException;

    public abstract void async_call_result(final IRpcResultCallback cb,
            final String m, final String f, final String signature,
            final Object... args) throws RpcException;

    void send(final OtpErlangPid pid, final Object msg);

    void send(final String name, final Object msg);
}
