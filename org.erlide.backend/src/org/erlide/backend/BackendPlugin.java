package org.erlide.backend;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.RegistryFactory;
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
    public void start(final BundleContext bundleContext) throws Exception {
        BackendPlugin.context = bundleContext;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.osgi.framework.BundleActivator#stop(org.osgi.framework.BundleContext)
     */
    public void stop(final BundleContext bundleContext) throws Exception {
        BackendPlugin.context = null;
    }

    public static IExtensionPoint getCodepathExtension() {
        final IExtensionRegistry reg = Platform.getExtensionRegistry();
        return reg.getExtensionPoint(PLUGIN_ID, "codepath");
    }

    public static IConfigurationElement[] getCodepathConfigurationElements() {
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        return reg.getConfigurationElementsFor(PLUGIN_ID, "codepath");
    }

    public static IConfigurationElement[] getSourcepathConfigurationElements() {
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        return reg.getConfigurationElementsFor(PLUGIN_ID, "sourcePathProvider");
    }

    public static IConfigurationElement[] getMessageReporterConfigurationElements() {
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        return reg.getConfigurationElementsFor(PLUGIN_ID, "messageReporter");
    }
}
