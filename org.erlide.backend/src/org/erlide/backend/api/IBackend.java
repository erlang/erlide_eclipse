package org.erlide.backend.api;

import java.io.IOException;

import org.erlide.model.root.IErlProject;
import org.erlide.runtime.api.ICodeBundle;
import org.erlide.runtime.api.IErlRuntime;
import org.erlide.runtime.api.IRuntimeStateListener;

public interface IBackend extends IErlRuntime, IRuntimeStateListener {

    String getName();

    void initialize();

    BackendData getData();

    void registerCodeBundle(final ICodeBundle bundle);

    void unregisterCodeBundle(final ICodeBundle bundle);

    void input(final String s) throws IOException;

    void addProjectPath(final IErlProject project);

    void removeProjectPath(final IErlProject project);

    void registerEventHandler(Object handler);

}
