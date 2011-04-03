package org.erlide.core;

import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

public class CoreInjector {

    public static ErlangCore injectErlangCore(final CoreScope coreScope) {
        return new ErlangCore(coreScope.getPlugin(),
                coreScope.getBundleContext(), null);
    }

    public IWorkspace injectWorkspace() {
        return ResourcesPlugin.getWorkspace();
    }

    public IExtensionRegistry injectExtensionRegistry() {
        return Platform.getExtensionRegistry();
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
