package org.erlide.backend;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class BackendPlugin implements BundleActivator {

    public static final String PLUGIN_ID = "org.erlide.backend";

    // we need to refer to the old ids
    public static final String PREFS_ID = "org.erlide.core";

    private static BundleContext context;

    static BundleContext getContext() {
        return context;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.osgi.framework.BundleActivator#start(org.osgi.framework.BundleContext
     * )
     */
    public void start(BundleContext bundleContext) throws Exception {
        BackendPlugin.context = bundleContext;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
     */
    public void stop(BundleContext bundleContext) throws Exception {
        BackendPlugin.context = null;
    }

}
