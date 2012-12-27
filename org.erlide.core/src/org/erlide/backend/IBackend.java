package org.erlide.backend;

import java.io.IOException;

import org.eclipse.core.resources.IProject;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.erlide.backend.console.IBackendShell;
import org.erlide.core.model.root.IErlProject;
import org.erlide.runtime.IRpcSite;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.utils.IDisposable;
import org.osgi.framework.Bundle;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpMbox;

public interface IBackend extends IDisposable, IRpcSite {

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

    void addProjectPath(final IErlProject project);

    void removeProjectPath(final IErlProject project);

    boolean isManaged();

    boolean doLoadOnAllNodes();

    IStreamsProxy getStreamsProxy();

    boolean hasConsole();

    IBackendData getData();

    void initialize();

    void installDeferredBreakpoints();

    void interpret(IProject project, String moduleName, boolean distributed,
            boolean interpret);

}
