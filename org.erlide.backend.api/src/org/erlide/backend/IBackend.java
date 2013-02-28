package org.erlide.backend;

import java.io.IOException;

import org.erlide.model.root.IErlProject;
import org.erlide.runtime.ICodeBundle;
import org.erlide.runtime.IErlRuntime;
import org.erlide.runtime.IRuntimeStateListener;
import org.erlide.utils.IDisposable;

public interface IBackend extends IDisposable, IErlRuntime,
        IRuntimeStateListener {

    void initialize();

    BackendData getData();

    void registerCodeBundle(final ICodeBundle bundle);

    void unregisterCodeBundle(final ICodeBundle bundle);

    void input(final String s) throws IOException;

    void addProjectPath(final IErlProject project);

    void removeProjectPath(final IErlProject project);

}
