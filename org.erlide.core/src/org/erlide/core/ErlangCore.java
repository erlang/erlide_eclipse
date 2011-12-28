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
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.erlide.core.backend.BackendCore;
import org.erlide.core.backend.BackendUtils;
import org.erlide.core.common.CommonUtils;
import org.erlide.core.common.EncodingUtils;
import org.erlide.core.debug.ErlangDebugOptionsManager;
import org.erlide.core.internal.model.root.ErlModel;
import org.erlide.core.model.root.IErlModel;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.RpcMonitor;
import org.osgi.framework.Bundle;
import org.osgi.service.prefs.BackingStoreException;

public final class ErlangCore {
    public static final String PLUGIN_ID = "org.erlide.core";
    public static final String NATURE_ID = PLUGIN_ID + ".erlnature";
    public static final String BUILDER_ID = PLUGIN_ID + ".erlbuilder";
    private static String featureVersion;

    private final ErlLogger logger;
    private final Plugin plugin;
    private final IWorkspace workspace;
    private final IExtensionRegistry extensionRegistry;
    private ISaveParticipant saveParticipant;
    private final ErlangDebugOptionsManager erlangDebugOptionsManager;

    public ErlangCore(final Plugin plugin, final IWorkspace workspace,
            final IExtensionRegistry extensionRegistry, final String logDir,
            final ErlangDebugOptionsManager erlangDebugOptionsManager) {
        this.plugin = plugin;
        this.workspace = workspace;
        this.extensionRegistry = extensionRegistry;
        this.erlangDebugOptionsManager = erlangDebugOptionsManager;
        featureVersion = "?";

        logger = ErlLogger.getInstance();
        final String dir = getLogDir(logDir);
        log(Level.INFO, "Erlide log is in " + dir);
        logger.setLogDir(dir);

        try {
            // ignore result, just setup cache
            BackendUtils.getSourcePathProviders();
        } catch (final CoreException e) {
            // ignore
        }
    }

    public static String getLogDir(final String logDir) {
        final IPreferencesService service = Platform.getPreferencesService();
        final String key = "erlide_log_directory";
        final String pluginId = "org.erlide.core";
        final String s = service.getString(pluginId, key, logDir, null);
        String dir;
        if (s != null) {
            dir = s;
        } else {
            dir = System.getProperty("user.home");
        }
        return dir;
    }

    public void start(final String version) throws CoreException {
        ErlLogger.debug("Starting CORE " + Thread.currentThread());
        String dev = "(" + EncodingUtils.getEncoding() + ") ";
        if (CommonUtils.isDeveloper()) {
            dev += " developer version ***";
        }
        if (CommonUtils.isTest()) {
            dev += " test ***";
        }
        final String versionBanner = "*** starting Erlide v" + version
                + " *** " + dev;
        log(Level.INFO, versionBanner);
        featureVersion = version;

        workspace.addSaveParticipant(plugin.getBundle().getSymbolicName(),
                getSaveParticipant());

        BackendCore.getBackendManager().loadCodepathExtensions();

        ErlangDebugOptionsManager.getDefault().start();
        ErlLogger.debug("Started CORE");
    }

    public void stop() {
        getModel().shutdown();
        ErlangDebugOptionsManager.getDefault().shutdown();
        logger.dispose();
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

    public static String getFeatureVersion() {
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

    public ILog getLog() {
        return plugin.getLog();
    }

    public IPath getStateLocation() {
        return plugin.getStateLocation();
    }

    public static boolean hasFeatureEnabled(final String feature) {
        return Boolean.parseBoolean(System.getProperty(feature));
    }

    public static IErlModel getModel() {
        return ErlModel.getErlangModel();
    }

    public void log(final IStatus status) {
        final Level lvl = getLevelFromSeverity(status.getSeverity());
        logger.log(lvl, status.getMessage());
        final Throwable exception = status.getException();
        if (exception != null) {
            logger.log(lvl, exception);
        }
        plugin.getLog().log(status);
    }

    public void log(final Level lvl, final String status) {
        logger.log(lvl, status);
        plugin.getLog().log(
                new Status(getSeverityFromLevel(lvl), PLUGIN_ID, status));
    }

    private static Level getLevelFromSeverity(final int severity) {
        Level lvl;
        switch (severity) {
        case IStatus.ERROR:
            lvl = Level.SEVERE;
            break;
        case IStatus.WARNING:
            lvl = Level.WARNING;
            break;
        case IStatus.INFO:
            lvl = Level.INFO;
            break;
        default:
            lvl = Level.FINEST;
        }
        return lvl;
    }

    private static int getSeverityFromLevel(final Level lvl) {
        final int sev;
        if (lvl == Level.SEVERE) {
            sev = IStatus.ERROR;
        } else if (lvl == Level.WARNING) {
            sev = IStatus.WARNING;
        } else if (lvl == Level.INFO) {
            sev = IStatus.INFO;
        } else {
            sev = IStatus.OK;
        }
        return sev;
    }

    public void logErrorMessage(final String message) {
        log(new Status(IStatus.ERROR, plugin.getBundle().getSymbolicName(),
                ErlangStatus.INTERNAL_ERROR.getValue(), message, null));
    }

    public void logErrorStatus(final String message, final IStatus status) {
        if (status == null) {
            logErrorMessage(message);
            return;
        }
        final MultiStatus multi = new MultiStatus(plugin.getBundle()
                .getSymbolicName(), ErlangStatus.INTERNAL_ERROR.getValue(),
                message, null);
        multi.add(status);
        log(multi);
    }

    public void log(final Throwable e) {
        log(new Status(IStatus.ERROR, plugin.getBundle().getSymbolicName(),
                ErlangStatus.INTERNAL_ERROR.getValue(),
                "Erlide internal error", e));
    }

    public void log(final String msg, final Throwable thr) {
        final String id = plugin.getBundle().getSymbolicName();
        final Status status = new Status(IStatus.ERROR, id, IStatus.OK, msg,
                thr);
        plugin.getLog().log(status);
    }

    public void debug(final String message) {
        if (plugin.isDebugging()) {
            ErlLogger.debug(message);
        }
    }

}
