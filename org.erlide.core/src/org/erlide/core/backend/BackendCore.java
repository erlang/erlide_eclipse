package org.erlide.core.backend;

import org.erlide.core.backend.runtimeinfo.RuntimeInfo;
import org.erlide.core.backend.runtimeinfo.RuntimeInfoManager;
import org.erlide.core.internal.backend.BackendFactory;
import org.erlide.core.internal.backend.BackendManager;

public class BackendCore {

    private static IBackendFactory backendFactory;
    private static RuntimeInfoManager runtimeInfoManager;
    private static IBackendManager backendManager;

    public static final RuntimeInfoManager getRuntimeInfoManager() {
        if (runtimeInfoManager == null) {
            runtimeInfoManager = new RuntimeInfoManager();
        }
        return runtimeInfoManager;
    }

    private static final IBackendFactory getBackendFactory() {
        if (backendFactory == null) {
            backendFactory = new BackendFactory(getRuntimeInfoManager());
        }
        return backendFactory;
    }

    public static final IBackendManager getBackendManager() {
        if (backendManager == null) {
            final RuntimeInfo erlideRuntime = getRuntimeInfoManager()
                    .getErlideRuntime();
            backendManager = new BackendManager(erlideRuntime,
                    getBackendFactory());
        }
        return backendManager;
    }

}
