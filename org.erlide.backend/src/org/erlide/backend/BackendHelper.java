package org.erlide.backend;

import org.erlide.runtime.api.RuntimeHelper;

public class BackendHelper extends RuntimeHelper {

    public BackendHelper() {
        super(BackendCore.getBackendManager().getIdeBackend().getRpcSite());
    }

}
