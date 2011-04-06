package org.erlide.core;

import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.erlide.core.backend.manager.BackendManager;
import org.erlide.core.backend.manager.BackendService;

public class CoreInjector {

    public static ErlangCore injectErlangCore(final CoreScope coreScope) {
        return new ErlangCore(coreScope.getPlugin(), injectServiceMap(),
                injectWorkspace(), injectExtensionRegistry());
    }

    private static ServicesMap injectServiceMap() {
        ServicesMap result = new ServicesMap();
        result.putService(BackendService.class, new BackendService());
        return result;
    }

    public static IWorkspace injectWorkspace() {
        return ResourcesPlugin.getWorkspace();
    }

    public static IExtensionRegistry injectExtensionRegistry() {
        return Platform.getExtensionRegistry();
    }

    public static BackendManager injectBackendManager() {
        return new BackendManager();
    }

    // public static final IErlModelManager injectModelManager() {
    // return ErlModelManager.getDefault();
    // }
    //
    // public static final IErlModel injectModel(
    // final IErlModelManager modelManager) {
    // return modelManager.getErlangModel();
    // }

}
