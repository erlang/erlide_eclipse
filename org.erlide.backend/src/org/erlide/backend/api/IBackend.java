package org.erlide.backend.api;

import java.io.IOException;
import java.util.Collection;

import org.erlide.model.root.IErlProject;
import org.erlide.runtime.api.IErlRuntime;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.IShutdownCallback;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.shell.IBackendShell;
import org.erlide.util.IDisposable;

public interface IBackend extends IShutdownCallback, IDisposable {

    String getName();

    BackendData getData();

    RuntimeInfo getRuntimeInfo();

    IRpcSite getRpcSite();

    IErlRuntime getRuntime();

    boolean isRunning();

    IBackendShell getShell(String string);

    void initialize(Collection<ICodeBundle> collection);

    void input(final String s) throws IOException;

    void registerCodeBundle(final ICodeBundle bundle);

    void unregisterCodeBundle(final ICodeBundle bundle);

    void addProjectPath(final IErlProject project);

    void removeProjectPath(final IErlProject project);

}
