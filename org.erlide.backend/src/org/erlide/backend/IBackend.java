package org.erlide.backend;

import java.io.IOException;

import org.eclipse.core.resources.IProject;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IStreamsProxy;
import org.erlide.backend.console.IBackendShell;
import org.erlide.model.root.IErlProject;
import org.erlide.runtime.ICodeBundle;
import org.erlide.runtime.IErlRuntime;
import org.erlide.runtime.IRuntimeStateListener;
import org.erlide.utils.IDisposable;

public interface IBackend extends IDisposable, IErlRuntime,
        IRuntimeStateListener {

    void initialize();

    BackendData getData();

    boolean hasConsole();

    boolean isManaged();

    ILaunch getLaunch();

    IStreamsProxy getStreamsProxy();

    void registerCodeBundle(final ICodeBundle bundle);

    void unregisterCodeBundle(final ICodeBundle bundle);

    IBackendShell getShell(final String id);

    void input(final String s) throws IOException;

    void addProjectPath(final IErlProject project);

    void removeProjectPath(final IErlProject project);

    boolean shouldLoadOnAllNodes();

    void installDeferredBreakpoints();

    void interpret(IProject project, String moduleName, boolean distributed,
            boolean interpret);

}
