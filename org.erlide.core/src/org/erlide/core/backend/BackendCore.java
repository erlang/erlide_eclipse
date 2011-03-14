package org.erlide.core.backend;

import org.erlide.core.backend.manager.BackendFactory;
import org.erlide.core.backend.runtimeinfo.RuntimeInfoManager;

public class BackendCore {

    private static BackendFactory backendFactory;
    private static RuntimeInfoManager runtimeInfoManager;

    public static final RuntimeInfoManager getRuntimeInfoManager() {
        if (runtimeInfoManager == null) {
            runtimeInfoManager = RuntimeInfoManager.getDefault();
        }
        return runtimeInfoManager;
    }

    public static final BackendFactory getBackendFactory() {
        if (backendFactory == null) {
            backendFactory = new BackendFactory();
        }
        return backendFactory;
    }

}
