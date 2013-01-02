package org.erlide.backend;

import java.io.IOException;

import org.eclipse.core.resources.IProject;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.erlide.backend.console.IBackendShell;
import org.erlide.core.model.root.IErlProject;
import org.erlide.runtime.IErlRuntime;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.utils.IDisposable;
import org.osgi.framework.Bundle;

public interface IBackend extends IDisposable, IErlRuntime {

    String getErlangVersion();

    RuntimeInfo getRuntimeInfo();

    String getName();

    String getFullNodeName();

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
