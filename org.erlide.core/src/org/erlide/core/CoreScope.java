/*******************************************************************************
 * Copyright (c) 2009 * and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     *
 *******************************************************************************/
package org.erlide.core;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.erlide.core.model.erlang.IErlModel;
import org.erlide.core.model.erlang.IErlModelManager;
import org.erlide.core.model.erlang.internal.ErlModelManager;
import org.osgi.framework.BundleContext;

public class CoreScope {

    private final BundleContext bundleContext;
    private final ErlangPlugin plugin;

    public CoreScope(final ErlangPlugin plugin,
            final BundleContext bundleContext) {
        this.bundleContext = bundleContext;
        this.plugin = plugin;
    }

    public static final IErlModelManager getModelManager() {
        return ErlModelManager.getDefault();
    }

    public static final IErlModel getModel() {
        return getModelManager().getErlangModel();
    }

    /**
     * Returns the workspace root default charset encoding.
     * 
     * @return the name of the default charset encoding for workspace root.
     * @see IContainer#getDefaultCharset()
     * @see ResourcesPlugin#getEncoding()
     */
    public static String getEncoding() {
        // Verify that workspace is not shutting down (see bug
        // https://bugs.eclipse.org/bugs/show_bug.cgi?id=60687)
        final IWorkspace workspace = getWorkspace();
        if (workspace != null) {
            try {
                return workspace.getRoot().getDefaultCharset();
            } catch (final CoreException e) {
                // fails silently and return plugin global encoding if core
                // exception occurs
            }
        }
        return ResourcesPlugin.getEncoding();
    }

    public static IWorkspace getWorkspace() {
        return ResourcesPlugin.getWorkspace();
    }

    public BundleContext getBundleContext() {
        return bundleContext;
    }

    public ErlangPlugin getPlugin() {
        return plugin;
    }

    // TODO add config for System properties!

}
