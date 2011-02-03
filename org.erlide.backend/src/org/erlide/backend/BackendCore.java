package org.erlide.backend;

import org.erlide.backend.runtime.RuntimeInfoManager;

public class BackendCore {

    public static final RuntimeInfoManager getRuntimeInfoManager() {
        return RuntimeInfoManager.getDefault();
    }

}
