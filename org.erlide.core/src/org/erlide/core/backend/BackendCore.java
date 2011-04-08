package org.erlide.core.backend;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.RegistryFactory;
import org.erlide.core.ErlangPlugin;
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

    public static IConfigurationElement[] getSourcepathConfigurationElements() {
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        return reg.getConfigurationElementsFor(ErlangPlugin.PLUGIN_ID, "sourcePathProvider");
    }

    public static IConfigurationElement[] getCodepathConfigurationElements() {
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        return reg.getConfigurationElementsFor(ErlangPlugin.PLUGIN_ID, "codepath");
    }

    public static IExtensionPoint getCodepathExtension() {
        final IExtensionRegistry reg = Platform.getExtensionRegistry();
        return reg.getExtensionPoint(ErlangPlugin.PLUGIN_ID, "codepath");
    }

}
