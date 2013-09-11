package org.erlide.backend;

import org.eclipse.core.runtime.Plugin;
import org.erlide.backend.api.IBackendFactory;
import org.erlide.backend.api.IBackendManager;
import org.erlide.backend.internal.BackendFactory;
import org.erlide.backend.internal.BackendManager;
import org.erlide.runtime.runtimeinfo.IRuntimeInfoCatalog;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.util.DebugStream;
import org.osgi.framework.BundleContext;

public class BackendPlugin extends Plugin {

    public static final String PLUGIN_ID = "org.erlide.backend";
    private static BackendPlugin plugin;

    public BackendPlugin() {
        super();
        plugin = this;
    }

    public static BackendPlugin getDefault() {
        if (plugin == null) {
            plugin = new BackendPlugin();
        }
        return plugin;
    }

    @Override
    public void start(final BundleContext context) throws Exception {
        super.start(context);
        DebugStream.activate();

        final IRuntimeInfoCatalog catalog = BackendCore.getRuntimeInfoCatalog();
        final RuntimeInfo erlideRuntime = catalog.getErlideRuntime();
        final IBackendFactory backendFactory = new BackendFactory(catalog);
        final IBackendManager backendManager = new BackendManager(
                erlideRuntime, backendFactory);
        BackendCore.init(backendManager);
    }

    @Override
    public void stop(final BundleContext context) throws Exception {
        BackendCore.stop();
        super.stop(context);
    }

}
