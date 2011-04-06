/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core;

import org.eclipse.core.runtime.IBundleGroup;
import org.eclipse.core.runtime.IBundleGroupProvider;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.RegistryFactory;
import org.erlide.jinterface.ErlLogger;
import org.osgi.framework.BundleContext;
import org.osgi.framework.Version;

/**
 * The main plugin class to be used in the desktop.
 * 
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 * @author Vlad Dumitrescu [vladdu55 at gmail dot com]
 * @author jakob
 */

public class ErlangPlugin extends Plugin {
    public static final String PLUGIN_ID = "org.erlide.core";
    public static final String BUILDER_ID = PLUGIN_ID + ".erlbuilder";
    public static final String NATURE_ID = PLUGIN_ID + ".erlnature";

    private static ErlangPlugin plugin;
    private ErlangCore core;

    public ErlangPlugin() {
        super();
        plugin = this;
    }

    /**
     * Returns the shared instance.
     * 
     * @return The plugin
     */
    public static ErlangPlugin getDefault() {
        if (plugin == null) {
            plugin = new ErlangPlugin();
        }
        return plugin;
    }

    /*
     * (non-Edoc) Shutdown the ErlangCore plug-in. <p> De-registers the
     * ErlModelManager as a resource changed listener and save participant. <p>
     * 
     * @see org.eclipse.core.runtime.Plugin#stop(BundleContext)
     */
    @Override
    public void stop(final BundleContext context) throws Exception {
        try {
            core.stop();
        } finally {
            core = null;
            // ensure we call super.stop as the last thing
            super.stop(context);
            plugin = null;
        }
    }

    /*
     * (non-Edoc) Startup the ErlangCore plug-in. <p> Registers the
     * ErlModelManager as a resource changed listener and save participant.
     * Starts the background indexing, and restore saved classpath variable
     * values. <p> @throws Exception
     * 
     * @see org.eclipse.core.runtime.Plugin#start(BundleContext)
     */
    @Override
    public void start(final BundleContext context) throws Exception {
        final ErlangScope coreScope = new ErlangScope(this, context);
        core = CoreInjector.injectErlangCore(coreScope);
        core.init();
        super.start(context);
        core.start(getFeatureVersion());
    }

    public String getFeatureVersion() {
        String version = null;
        try {
            final IBundleGroupProvider[] providers = Platform
                    .getBundleGroupProviders();
            if (providers != null) {
                version = findErlideFeatureVersion(providers);
            } else {
                ErlLogger.debug("***: no bundle group providers");
            }
        } catch (final Throwable e) {
            // ignore
        }
        final Version coreVersion = getBundle().getVersion();
        version = version == null ? "?" : version;
        version = version + " (core=" + coreVersion.toString() + ")";
        return version;
    }

    private String findErlideFeatureVersion(
            final IBundleGroupProvider[] providers) {
        String version = null;
        for (final IBundleGroupProvider provider : providers) {
            final IBundleGroup[] bundleGroups = provider.getBundleGroups();
            for (final IBundleGroup group : bundleGroups) {
                final String id = group.getIdentifier();
                if ("org.erlide".equals(id) || "org.erlide.headless".equals(id)) {
                    version = group.getVersion();
                    break;
                }
            }
            if (version != null) {
                break;
            }
        }
        return version;
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
