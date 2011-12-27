package org.erlide.core;

import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.erlide.core.backend.IBackendManager;
import org.erlide.core.internal.backend.BackendManager;
import org.erlide.core.internal.backend.BackendService;

public class CoreInjector {

    public static ErlangCore injectErlangCore(final ErlangPlugin plugin) {
        return new ErlangCore(plugin, injectServiceMap(), injectWorkspace(),
                injectExtensionRegistry(), injectCoreLogDir());
    }

    private static String injectCoreLogDir() {
        return ResourcesPlugin.getWorkspace().getRoot().getLocation()
                .toPortableString();
    }

    private static ServicesMap injectServiceMap() {
        final ServicesMap result = new ServicesMap();
        result.putService(BackendService.class, new BackendService());
        return result;
    }

    public static IWorkspace injectWorkspace() {
        return ResourcesPlugin.getWorkspace();
    }

    public static IExtensionRegistry injectExtensionRegistry() {
        return Platform.getExtensionRegistry();
    }

    public static IBackendManager injectBackendManager() {
        return new BackendManager();
    }

}
