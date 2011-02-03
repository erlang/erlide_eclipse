package org.erlide.backend;

import org.erlide.jinterface.backend.RuntimeInfoManager;

public class BackendCore {

    public static final RuntimeInfoManager getRuntimeInfoManager() {
        return RuntimeInfoManager.getDefault();
    }

}
