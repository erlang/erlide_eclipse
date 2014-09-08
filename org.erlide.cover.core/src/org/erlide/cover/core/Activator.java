package org.erlide.cover.core;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

public class Activator extends Plugin implements Logger {

    // The plug-in ID
    public static final String PLUGIN_ID = "org.erlide.cover.core"; //$NON-NLS-1$

    private static Activator plugin;

    @Override
    public void start(final BundleContext bundleContext) throws Exception {
        super.start(bundleContext);
        plugin = this;
    }

    @Override
    public void stop(final BundleContext bundleContext) throws Exception {
        super.stop(bundleContext);
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
