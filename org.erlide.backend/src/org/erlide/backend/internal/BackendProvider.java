package org.erlide.backend.internal;

import org.eclipse.core.resources.IProject;
import org.erlide.backend.BackendCore;
import org.erlide.backend.api.IBackendProvider;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.RuntimeVersion;

public class BackendProvider implements IBackendProvider {

    @Override
    public IRpcSite get() {
        return BackendCore.getBackendManager().getIdeBackend().getRpcSite();
    }

    @Override
    public IRpcSite get(final RuntimeVersion version) {
        return BackendCore.getBackendManager().getByVersion(version);
    }

    @Override
    public IRpcSite get(final IProject project) {
        return BackendCore.getBackendManager().getByProject(project);
    }
}
