package org.erlide.core.backend.manager;

import org.erlide.core.backend.BackendData;
import org.erlide.core.backend.IBackend;
import org.erlide.core.backend.runtimeinfo.RuntimeInfo;

public interface IBackendFactory {

    IBackend createIdeBackend();

    IBackend createBuildBackend(final RuntimeInfo info);

    IBackend createBackend(final BackendData data);

}
