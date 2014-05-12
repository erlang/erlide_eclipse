package org.erlide.backend.internal;

import org.erlide.backend.BackendCore;
import org.erlide.backend.api.IBackend;
import org.erlide.backend.api.IBackendManager;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.IRpcSiteProvider;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;

public class BackendProvider implements IRpcSiteProvider {

    @Override
    public IRpcSite get() {
        final IBackendManager backendManager = BackendCore.getBackendManager();
        final IBackend ideBackend = backendManager.getIdeBackend();
        return ideBackend.getRpcSite();
    }

    @Override
    public IRpcSite get(final RuntimeVersion version) {
        return BackendCore.getBackendManager().getByVersion(version);
    }

    @Override
    public IRpcSite get(final String projectName) {
        return BackendCore.getBackendManager().getByProject(projectName);
    }
}
