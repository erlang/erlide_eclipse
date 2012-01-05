package org.erlide.core.backend;

import org.erlide.core.backend.runtimeinfo.RuntimeInfo;
import org.erlide.core.backend.runtimeinfo.RuntimeInfoManager;
import org.erlide.core.internal.backend.BackendFactory;
import org.erlide.core.internal.backend.BackendManager;

public class BackendCore {

    private static RuntimeInfoManager runtimeInfoManager;
    private static IBackendManager backendManager;

    public static final RuntimeInfoManager getRuntimeInfoManager() {
        if (runtimeInfoManager == null) {
            runtimeInfoManager = new RuntimeInfoManager();
        }
        return runtimeInfoManager;
    }

    public static final IBackendManager getBackendManager() {
        if (backendManager == null) {
            final RuntimeInfo erlideRuntime = getRuntimeInfoManager()
                    .getErlideRuntime();
            final BackendFactory backendFactory = new BackendFactory(
                    getRuntimeInfoManager());
            backendManager = new BackendManager(erlideRuntime, backendFactory);
        }
        return backendManager;
    }
}
