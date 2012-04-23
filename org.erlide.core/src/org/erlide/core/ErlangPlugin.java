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

import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.erlide.launch.debug.ErlangDebugOptionsManager;
import org.osgi.framework.BundleContext;

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
        super.start(context);

        // final Bundle b = Platform.getBundle("org.eclipse.equinox.event");
        // if (b != null && b.getState() == Bundle.RESOLVED) {
        // try {
        // b.start();
        // } catch (final BundleException e) {
        // }
        // }

        final IWorkspace workspace = ResourcesPlugin.getWorkspace();
        final IExtensionRegistry extensionRegistry = Platform
                .getExtensionRegistry();
        final String portableString = workspace.getRoot().getLocation()
                .toPortableString();
        final ErlangDebugOptionsManager erlangDebugOptionsManager = ErlangDebugOptionsManager
                .getDefault();

        core = new ErlangCore(this, workspace, extensionRegistry,
                portableString, erlangDebugOptionsManager);
        core.start();
    }

    public ErlangCore getCore() {
        return core;
    }
}
