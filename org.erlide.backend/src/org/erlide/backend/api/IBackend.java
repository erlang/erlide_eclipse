package org.erlide.backend.api;

import java.io.IOException;
import java.util.Collection;

import org.erlide.model.root.IErlProject;
import org.erlide.runtime.api.ICodeBundle;
import org.erlide.runtime.api.IErlRuntime;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.IShutdownCallback;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.shell.IBackendShell;
import org.erlide.util.IDisposable;

public interface IBackend extends IShutdownCallback, IDisposable {

    String getName();

    void initialize(Collection<ICodeBundle> collection);

    BackendData getData();

    IRpcSite getRpcSite();

    void registerCodeBundle(final ICodeBundle bundle);

    void unregisterCodeBundle(final ICodeBundle bundle);

    void input(final String s) throws IOException;

    void addProjectPath(final IErlProject project);

    void removeProjectPath(final IErlProject project);

    RuntimeInfo getRuntimeInfo();

    boolean isRunning();

    IErlRuntime getRuntime();

    IBackendShell getShell(String string);

}
