/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation
 *******************************************************************************/
package org.erlide.core;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Level;

import org.eclipse.core.resources.ISaveContext;
import org.eclipse.core.resources.ISaveParticipant;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IBundleGroup;
import org.eclipse.core.runtime.IBundleGroupProvider;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.erlide.backend.BackendUtils;
import org.erlide.core.services.builder.BuildQueueProcessor;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.RpcMonitor;
import org.erlide.launch.debug.ErlangDebugOptionsManager;
import org.erlide.utils.EncodingUtils;
import org.erlide.utils.SystemUtils;
import org.osgi.framework.Bundle;
import org.osgi.framework.Version;
import org.osgi.service.prefs.BackingStoreException;

public final class ErlangCore {
    public static final String PLUGIN_ID = "org.erlide.core";
    public static final String NATURE_ID = PLUGIN_ID + ".erlnature";
    public static final String BUILDER_ID = PLUGIN_ID + ".erlbuilder";
    public static final String ERLIDE_GLOBAL_TRACE_OPTION = "org.erlide.backend/debug";

    private String featureVersion;

    private final Plugin plugin;
    private final IWorkspace workspace;
    private final IExtensionRegistry extensionRegistry;
    private ISaveParticipant saveParticipant;
    private final ErlangDebugOptionsManager erlangDebugOptionsManager;
    private final ErlangCoreLogger logger;

    public ErlangCore(final Plugin plugin, final IWorkspace workspace,
            final IExtensionRegistry extensionRegistry, final String logDir,
            final ErlangDebugOptionsManager erlangDebugOptionsManager) {
        this.plugin = plugin;
        this.workspace = workspace;
        this.extensionRegistry = extensionRegistry;
        this.erlangDebugOptionsManager = erlangDebugOptionsManager;
        featureVersion = "?";
        logger = new ErlangCoreLogger(plugin, logDir);

        // TODO can we remove this from here?
        try {
            // ignore result, just setup cache
            BackendUtils.getSourcePathProviders();
        } catch (final CoreException e) {
            // ignore
        }
    }

    public void start() throws CoreException {
        final String version = getFeatureVersionImpl();

        ErlLogger.debug("Starting CORE " + Thread.currentThread());
        String dev = "(" + EncodingUtils.getEncoding() + ") ";
        if (SystemUtils.getInstance().isDeveloper()) {
            dev += " developer version ***";
        }
        if (SystemUtils.getInstance().isTest()) {
            dev += " test ***";
        }
        final String versionBanner = "*** starting Erlide v" + version
                + " *** " + dev;
        logger.log(Level.INFO, versionBanner);
        featureVersion = version;

        workspace.addSaveParticipant(plugin.getBundle().getSymbolicName(),
                getSaveParticipant());

        ErlangDebugOptionsManager.getDefault().start();
        ErlLogger.debug("Started CORE");
    }

    public void stop() {
        BuildQueueProcessor.getInstance().stop();
        ErlangDebugOptionsManager.getDefault().shutdown();
        final String location = ResourcesPlugin.getWorkspace().getRoot()
                .getLocation().toPortableString();

        final String dateNow = new SimpleDateFormat("yyyyMMdd_HHmmss")
                .format(new Date());

        RpcMonitor.cleanupOldLogs(location, "rpc_monitor");
        RpcMonitor.dump(location + "/rpc_monitor-" + dateNow + ".dump");
    }

    public IWorkspace getWorkspace() {
        return workspace;
    }

    public IExtensionRegistry getExtensionRegistry() {
        return extensionRegistry;
    }

    public String getFeatureVersion() {
        return featureVersion;
    }

    public ISaveParticipant getSaveParticipant() {
        if (saveParticipant == null) {
            saveParticipant = new ISaveParticipant() {
                @Override
                public void doneSaving(final ISaveContext context1) {
                }

                @Override
                public void prepareToSave(final ISaveContext context1)
                        throws CoreException {
                }

                @Override
                public void rollback(final ISaveContext context1) {
                }

                @Override
                public void saving(final ISaveContext context1)
                        throws CoreException {
                    try {
                        new InstanceScope().getNode(
                                plugin.getBundle().getSymbolicName()).flush();
                    } catch (final BackingStoreException e) {
                        // ignore
                    }
                }
            };
        }
        return saveParticipant;
    }

    public boolean isDebugging() {
        return plugin.isDebugging();
    }

    public Bundle getBundle() {
        return plugin.getBundle();
    }

    public IPath getStateLocation() {
        return plugin.getStateLocation();
    }

    public boolean isTracing(final String traceOption) {
        if (!Platform.inDebugMode()) {
            return false;
        }
        final String globalTraceValue = Platform
                .getDebugOption(ERLIDE_GLOBAL_TRACE_OPTION);
        final String value = Platform.getDebugOption(ERLIDE_GLOBAL_TRACE_OPTION
                + "/" + traceOption);
        if ("true".equalsIgnoreCase(globalTraceValue)
                && "true".equalsIgnoreCase(value)) {
            return true;
        }
        return false;
    }

    private String getFeatureVersionImpl() {
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
            if (!version.equals("?")) {
                break;
            }
        }
        return version;
    }
}
