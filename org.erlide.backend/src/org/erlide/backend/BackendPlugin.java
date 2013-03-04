package org.erlide.backend;

import org.eclipse.core.runtime.Plugin;
import org.erlide.util.DebugStream;
import org.osgi.framework.BundleContext;

public class BackendPlugin extends Plugin {

    public static final String PLUGIN_ID = "org.erlide.backend";
    private static BackendPlugin plugin;
    private BundleContext bundleContext;

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
        bundleContext = context;
        DebugStream.activate();
    }

    @Override
    public void stop(final BundleContext context) throws Exception {
        super.stop(context);
    }

    public BundleContext getBundleContext() {
        return bundleContext;
    }

}
