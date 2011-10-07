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

import java.util.logging.Level;

import org.eclipse.core.resources.ISaveContext;
import org.eclipse.core.resources.ISaveParticipant;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.RegistryFactory;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.erlide.core.backend.BackendCore;
import org.erlide.core.backend.BackendUtils;
import org.erlide.core.backend.runtimeinfo.RuntimeInfoInitializer;
import org.erlide.core.common.CommonUtils;
import org.erlide.core.debug.ErlangDebugOptionsManager;
import org.erlide.jinterface.ErlLogger;
import org.osgi.framework.Bundle;
import org.osgi.service.prefs.BackingStoreException;

public final class ErlangCore {
    public static final String PLUGIN_ID = "org.erlide.core";
    public static final String NATURE_ID = PLUGIN_ID + ".erlnature";
    public static final String BUILDER_ID = PLUGIN_ID + ".erlbuilder";
    private static String featureVersion;

    private final ErlLogger logger;
    private final ServicesMap services;
    private final Plugin plugin;
    private final IWorkspace workspace;
    private final IExtensionRegistry extensionRegistry;
    private final ISaveParticipant saveParticipant;

    public ErlangCore(final Plugin plugin, final ServicesMap services,
            final IWorkspace workspace,
            final IExtensionRegistry extensionRegistry, final String logDir) {
        this.services = services;
        this.plugin = plugin;
        this.workspace = workspace;
        this.extensionRegistry = extensionRegistry;
        featureVersion = "?";
        saveParticipant = new ISaveParticipant() {
            public void doneSaving(final ISaveContext context1) {
            }

            public void prepareToSave(final ISaveContext context1)
                    throws CoreException {
            }

            public void rollback(final ISaveContext context1) {
            }

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
        final IPreferencesService service = Platform.getPreferencesService();
        final String key = "erlide_log_directory";
        final String pluginId = "org.erlide.core";
        final String s = service.getString(pluginId, key,
                System.getProperty("user.home"), null);
        String dir;
        if (s != null) {
            dir = s;
        } else {
            dir = logDir;
        }

        logger = ErlLogger.getInstance();
        logger.setLogDir(dir);

        try {
            // ignore result, just setup cache
            BackendUtils.getSourcePathProviders();
        } catch (final CoreException e) {
            // ignore
        }
    }

    public void stop() {
        CoreScope.getModel().shutdown();
        ErlangDebugOptionsManager.getDefault().shutdown();
        logger.dispose();
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

    public void start(final String version) throws CoreException {
        ErlLogger.debug("Starting CORE " + Thread.currentThread());
        String dev = "";
        if (CommonUtils.isDeveloper()) {
            dev = " erlide developer version ***";
        }
        if (CommonUtils.isTest()) {
            dev += " test ***";
        }
        ErlLogger.info("*** starting Erlide v" + version + " ***" + dev);
        featureVersion = version;

        final RuntimeInfoInitializer runtimeInfoInitializer = new RuntimeInfoInitializer(
                BackendCore.getRuntimeInfoManager());
        runtimeInfoInitializer.initializeRuntimesList();
        BackendCore.getBackendManager().loadCodepathExtensions();

        ErlangDebugOptionsManager.getDefault().start();
        ErlLogger.debug("Started CORE");
    }

    public IWorkspace getWorkspace() {
        return workspace;
    }

    public IExtensionRegistry getExtensionRegistry() {
        return extensionRegistry;
    }

    public static IConfigurationElement[] getMessageReporterConfigurationElements() {
        final IExtensionRegistry reg = RegistryFactory.getRegistry();
        return reg.getConfigurationElementsFor(ErlangCore.PLUGIN_ID,
                "messageReporter");
    }

    /**
     * Runs the given action as an atomic Erlang model operation.
     * <p>
     * After running a method that modifies Erlang elements, registered
     * listeners receive after-the-fact notification of what just transpired, in
     * the form of a element changed event. This method allows clients to call a
     * number of methods that modify Erlang elements and only have element
     * changed event notifications reported at the end of the entire batch.
     * </p>
     * <p>
     * If this method is called outside the dynamic scope of another such call,
     * this method runs the action and then reports a single element changed
     * event describing the net effect of all changes done to Erlang elements by
     * the action.
     * </p>
     * <p>
     * If this method is called in the dynamic scope of another such call, this
     * method simply runs the action.
     * </p>
     * 
     * @param action
     *            the action to perform
     * @param monitor
     *            a progress monitor, or <code>null</code> if progress reporting
     *            and cancellation are not desired
     * @throws CoreException
     *             if the operation failed.
     */
    public static void run(final IWorkspaceRunnable action,
            final IProgressMonitor monitor) throws CoreException {
        run(action, ResourcesPlugin.getWorkspace().getRoot(), monitor);
    }

    /**
     * Runs the given action as an atomic Erlang model operation.
     * <p>
     * After running a method that modifies Erlang elements, registered
     * listeners receive after-the-fact notification of what just transpired, in
     * the form of a element changed event. This method allows clients to call a
     * number of methods that modify Erlang elements and only have element
     * changed event notifications reported at the end of the entire batch.
     * </p>
     * <p>
     * If this method is called outside the dynamic scope of another such call,
     * this method runs the action and then reports a single element changed
     * event describing the net effect of all changes done to Erlang elements by
     * the action.
     * </p>
     * <p>
     * If this method is called in the dynamic scope of another such call, this
     * method simply runs the action.
     * </p>
     * <p>
     * The supplied scheduling rule is used to determine whether this operation
     * can be run simultaneously with workspace changes in other threads. See
     * <code>IWorkspace.run(...)</code> for more details.
     * </p>
     * 
     * @param action
     *            the action to perform
     * @param rule
     *            the scheduling rule to use when running this operation, or
     *            <code>null</code> if there are no scheduling restrictions for
     *            this operation.
     * @param monitor
     *            a progress monitor, or <code>null</code> if progress reporting
     *            and cancellation are not desired
     * @throws CoreException
     *             if the operation failed.
     */
    public static void run(final IWorkspaceRunnable action,
            final ISchedulingRule rule, final IProgressMonitor monitor)
            throws CoreException {
        final IWorkspace workspace = ResourcesPlugin.getWorkspace();
        if (workspace.isTreeLocked()) {
            // new BatchOperation(action).run(monitor);
        } else {
            // use IWorkspace.run(...) to ensure that a build will be done in
            // autobuild mode
            // workspace.run(new BatchOperation(action), rule,
            // IWorkspace.AVOID_UPDATE, monitor);
        }
    }

    public static String getFeatureVersion() {
        return featureVersion;
    }

    public ISaveParticipant getSaveParticipant() {
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
}
