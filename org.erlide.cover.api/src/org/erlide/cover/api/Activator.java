package org.erlide.cover.api;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class Activator implements BundleActivator {

    private static BundleContext context;

    static BundleContext getContext() {
        return context;
    }

    public void start(final BundleContext bundleContext) throws Exception {
        Activator.context = bundleContext;
    }

    public void stop(final BundleContext bundleContext) throws Exception {
        Activator.context = null;
    }

}
