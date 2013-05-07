package org.erlide.backend.internal;

import org.erlide.backend.BackendCore;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.IRuntimeProvider;
import org.erlide.runtime.api.RuntimeVersion;

public class BackendProvider implements IRuntimeProvider {

    @Override
    public IRpcSite get() {
        return BackendCore.getBackendManager().getIdeBackend().getRpcSite();
    }

    @Override
    public IRpcSite get(final RuntimeVersion version) {
        return BackendCore.getBackendManager().getByVersion(version);
    }

    @Override
    public IRpcSite get(final String project) {
        return BackendCore.getBackendManager().getByProject(project);
    }
}
