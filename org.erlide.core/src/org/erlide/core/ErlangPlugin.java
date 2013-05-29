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
import org.erlide.util.ErlideEventTracer;
import org.erlide.util.FileEventTracer;
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
    private FileEventTracer handler;

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
            ErlideEventTracer.doStop(context);
            handler.dispose();

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

        startEventTracer(context);

        final IWorkspace workspace = ResourcesPlugin.getWorkspace();
        final IExtensionRegistry extensionRegistry = Platform
                .getExtensionRegistry();
        final String logDir = workspace.getRoot().getLocation()
                .toPortableString();
        final ErlangDebugOptionsManager erlangDebugOptionsManager = ErlangDebugOptionsManager
                .getDefault();

        ErlideEventTracer.traceSession();
        ErlideEventTracer.traceCrash("hello");

        core = new ErlangCore(this, workspace, extensionRegistry, logDir,
                erlangDebugOptionsManager);
        core.start();
    }

    private void startEventTracer(final BundleContext context) {
        final String tracerPath = System.getProperty("erlide.event_tracer");
        if (tracerPath == null) {
            return;
        }
        handler = new FileEventTracer(tracerPath);
        ErlideEventTracer.registerHandler(handler, context);
        ErlideEventTracer.doStart(context);
    }

    public ErlangCore getCore() {
        return core;
    }
}
