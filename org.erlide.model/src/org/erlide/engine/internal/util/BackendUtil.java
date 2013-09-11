package org.erlide.engine.internal.util;

import org.eclipse.core.resources.IProject;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.IRpcSiteProvider;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.services.ExtensionUtils;

public class BackendUtil {

    public IRpcSite getBackend(final RuntimeVersion version) {
        final IRpcSiteProvider provider = getRuntimeProvider();
        return provider.get(version);
    }

    public IRpcSite getBackend(final IProject project) {
        final IRpcSiteProvider provider = getRuntimeProvider();
        return provider.get(project.getName());
    }

    private IRpcSiteProvider getRuntimeProvider() {
        return ExtensionUtils.getSingletonExtension(
                "org.erlide.backend.backend", IRpcSiteProvider.class);
    }

}
