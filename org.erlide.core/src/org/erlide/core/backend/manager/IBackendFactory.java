package org.erlide.core.backend.manager;

import org.erlide.core.backend.Backend;
import org.erlide.core.backend.BackendData;
import org.erlide.core.backend.runtimeinfo.RuntimeInfo;

public interface IBackendFactory {

    Backend createIdeBackend();

    Backend createBuildBackend(final RuntimeInfo info);

    Backend createBackend(final BackendData data);

}
