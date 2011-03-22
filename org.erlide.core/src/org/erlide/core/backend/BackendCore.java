package org.erlide.core.backend;

import org.erlide.core.backend.manager.BackendFactory;
import org.erlide.core.backend.manager.BackendManager;
import org.erlide.core.backend.runtimeinfo.RuntimeInfoManager;

public class BackendCore {

    private static BackendFactory backendFactory;
    private static RuntimeInfoManager runtimeInfoManager;
    private static BackendManager backendManager;

    public static final RuntimeInfoManager getRuntimeInfoManager() {
        if (runtimeInfoManager == null) {
            runtimeInfoManager = new RuntimeInfoManager();
        }
        return runtimeInfoManager;
    }

    public static final BackendFactory getBackendFactory() {
        if (backendFactory == null) {
            backendFactory = new BackendFactory(getRuntimeInfoManager());
        }
        return backendFactory;
    }

    public static final BackendManager getBackendManager() {
        if (backendManager == null) {
            backendManager = new BackendManager();
        }
        return backendManager;
    }

}
