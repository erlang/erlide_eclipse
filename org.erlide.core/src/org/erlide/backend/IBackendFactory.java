package org.erlide.backend;

import org.erlide.jinterface.runtimeinfo.RuntimeInfo;

public interface IBackendFactory {

    IBackend createIdeBackend();

    IBackend createBuildBackend(final RuntimeInfo info);

    IBackend createBackend(final IBackendData data);

}
