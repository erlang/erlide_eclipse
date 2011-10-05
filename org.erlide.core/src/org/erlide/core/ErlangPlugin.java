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

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IBundleGroup;
import org.eclipse.core.runtime.IBundleGroupProvider;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
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
    private static ErlangPlugin plugin;
    private ErlangCore core;

    public ErlangPlugin() {
        super();
        plugin = this;
    }

    public static ErlangPlugin getDefault() {
        if (plugin == null) {
            plugin = new ErlangPlugin();
        }
        return plugin;
    }

    @Override
    public void stop(final BundleContext context) throws Exception {
        try {
            ResourcesPlugin.getWorkspace().removeSaveParticipant(
                    getBundle().getSymbolicName());
            if (core != null) {
                core.stop();
            }
        } finally {
            core = null;
            plugin = null;
            // ensure we call super.stop as the last thing
            super.stop(context);
        }
    }

    @Override
    public void start(final BundleContext context) throws Exception {
        final CoreScope coreScope = new CoreScope(this, context);
        core = CoreInjector.injectErlangCore(coreScope);
        super.start(context);
        ResourcesPlugin.getWorkspace().addSaveParticipant(
                getBundle().getSymbolicName(), core.getSaveParticipant());

        core.start(getFeatureVersion());
    }

    private String getFeatureVersion() {
        String version = "?";
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
        version = version + " (core=" + coreVersion.toString() + ")";
        return version;
    }

    private String findErlideFeatureVersion(
            final IBundleGroupProvider[] providers) {
        String version = "?";
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

}
