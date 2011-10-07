package org.erlide.core.backend;

import java.io.IOException;

import org.eclipse.core.resources.IProject;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.erlide.core.backend.console.IBackendShell;
import org.erlide.core.backend.runtimeinfo.RuntimeInfo;
import org.erlide.core.common.IDisposable;
import org.erlide.core.rpc.IRpcCallSite;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNodeStatus;

public interface IBackend extends IRpcCallSite, IDisposable {

    IRpcCallSite getCallSite();

    OtpErlangObject receiveEvent(final long timeout) throws OtpErlangExit,
            OtpErlangDecodeException;

    void connect();

    String getErlangVersion();

    OtpErlangPid getEventPid();

    RuntimeInfo getRuntimeInfo();

    String getName();

    String getFullNodeName();

    boolean isStopped();

    void registerStatusHandler(final OtpNodeStatus handler);

    void stop();

    OtpMbox createMbox();

    OtpMbox createMbox(final String name);

    void removePath(final String path);

    void addPath(final boolean usePathZ, final String path);

    void initErlang(final boolean monitor, final boolean watch);

    void register(final ICodeBundle bundle);

    void unregister(final Bundle b);

    ILaunch getLaunch();

    void setLaunch(final ILaunch launch);

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

    void launchRuntime();

    String getJavaNodeName();

    void installDeferredBreakpoints();

}
