package org.erlide.backend;

import org.erlide.backend.internal.BackendFactory;
import org.erlide.backend.internal.BackendManager;
import org.erlide.backend.runtimeinfo.RuntimeInfo;
import org.erlide.backend.runtimeinfo.RuntimeInfoManager;

public class BackendCore {

    private static RuntimeInfoManager runtimeInfoManager;
    private static IBackendManager backendManager;
    private static BackendFactory backendFactory;

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
            backendFactory = new BackendFactory(getRuntimeInfoManager());
            backendManager = new BackendManager(erlideRuntime, backendFactory);
        }
        return backendManager;
    }

}
