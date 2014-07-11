package org.erlide.engine.util;

import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.api.IRpcSite;
import org.erlide.runtime.api.IRpcSiteProvider;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.services.ExtensionUtils;

public class RpcSiteFactory {

    private static IRpcSiteProvider provider;

    public static IRpcSite getRpcSite(final RuntimeVersion version) {
        if (provider == null) {
            provider = getRpcSiteProvider();
        }
        return provider.get(version);
    }

    public static IRpcSite getRpcSiteForProject(final IErlProject project) {
        if (provider == null) {
            provider = getRpcSiteProvider();
        }
        return provider.get(project.getName());
    }

    public static IRpcSite getRpcSite() {
        if (provider == null) {
            provider = getRpcSiteProvider();
        }
        return provider.get();
    }

    private static IRpcSiteProvider getRpcSiteProvider() {
        return ExtensionUtils.getSingletonExtension("org.erlide.backend.backend",
                IRpcSiteProvider.class);
    }

}
