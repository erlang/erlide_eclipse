package org.erlide.engine.internal;

import org.eclipse.core.runtime.Plugin;
import org.erlide.util.ErlLogger;
import org.osgi.framework.BundleContext;

public class ModelPlugin extends Plugin {

    public static final String PLUGIN_ID = "org.erlide.model";
    // FIXME this is kind of an indirect dep on core plugin (needs to be
    // started)
    private static final String CORE_PLUGIN_ID = "org.erlide.core";
    public static final String NATURE_ID = CORE_PLUGIN_ID + ".erlnature";
    public static final String BUILDER_ID = CORE_PLUGIN_ID + ".erlbuilder";

    private static BundleContext context;
    static ModelPlugin plugin;

    public ModelPlugin() {
        super();
        plugin = this;
    }

    public static ModelPlugin getDefault() {
        if (plugin == null) {
            plugin = new ModelPlugin();
        }
        return plugin;
    }

    static BundleContext getContext() {
        return context;
    }

    @Override
    public void start(final BundleContext bundleContext) throws Exception {
        ErlLogger.debug("Starting Erlang model");
        super.start(bundleContext);
        ModelPlugin.context = bundleContext;
        ErlLogger.debug("Started model");
    }

    @Override
    public void stop(final BundleContext bundleContext) throws Exception {
        ModelPlugin.context = null;
        super.stop(bundleContext);
    }

}
