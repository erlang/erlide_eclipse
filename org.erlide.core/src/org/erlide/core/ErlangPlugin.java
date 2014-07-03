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
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.erlide.backend.debug.ErlangDebugOptionsManager;
import org.erlide.util.ErlLogger;
import org.erlide.util.event_tracer.ErlideEventTracer;
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
    private boolean stopping = false;

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
        stopping = true;
        try {
            ErlideEventTracer.getInstance().dispose();

            final IWorkspace workspace = ResourcesPlugin.getWorkspace();
            workspace.removeSaveParticipant(getBundle().getSymbolicName());

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
        ErlLogger.debug("Core starting");

        final IWorkspace workspace = ResourcesPlugin.getWorkspace();
        final IWorkspaceRoot workspaceRoot = workspace.getRoot();
        ErlideEventTracer.getInstance().traceSession(
                workspaceRoot.getLocation().toPortableString());

        final IExtensionRegistry extensionRegistry = Platform.getExtensionRegistry();
        final String logDir = workspaceRoot.getLocation().toPortableString();
        final ErlangDebugOptionsManager erlangDebugOptionsManager = new ErlangDebugOptionsManager();

        core = new ErlangCore(this, workspace, extensionRegistry, logDir,
                erlangDebugOptionsManager);
        core.start();

        ErlLogger.debug("Core started");
    }

    public ErlangCore getCore() {
        return core;
    }

    public boolean isStopping() {
        return stopping;
    }
}
