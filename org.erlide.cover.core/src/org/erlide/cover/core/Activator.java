package org.erlide.cover.core;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

public class Activator extends Plugin implements Logger {

    // The plug-in ID
    public static final String PLUGIN_ID = "org.erlide.cover.core"; //$NON-NLS-1$

    private static BundleContext context;

    private static Activator plugin;

    static BundleContext getContext() {
        return context;
    }

    @Override
    public void start(final BundleContext bundleContext) throws Exception {
        Activator.context = bundleContext;
        plugin = this;
    }

    @Override
    public void stop(final BundleContext bundleContext) throws Exception {
        Activator.context = null;
    }

    @Override
    public void info(final Object msg) {
        LogUtils.log(this, IStatus.INFO, msg, 1);
    }

    @Override
    public void error(final Object msg) {
        LogUtils.log(this, IStatus.ERROR, msg, 1);
    }

    public static Activator getDefault() {
        return plugin;
    }

}
