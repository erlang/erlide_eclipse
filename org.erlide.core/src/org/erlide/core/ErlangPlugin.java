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

import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ISaveContext;
import org.eclipse.core.resources.ISaveParticipant;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IBundleGroup;
import org.eclipse.core.runtime.IBundleGroupProvider;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.core.platform.PlatformChangeListener;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.BackendManager;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.InvalidSyntaxException;
import org.osgi.framework.ServiceReference;
import org.osgi.framework.ServiceRegistration;
import org.osgi.framework.Version;
import org.osgi.service.prefs.BackingStoreException;

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
    private ResourceBundle resourceBundle;
    private PlatformChangeListener platformListener;
    // this must be here, otherwise ErlideLogger messages look weird
    @SuppressWarnings("unused")
    private Logger logger;

    public ErlangPlugin() {
        super();
        plugin = this;
        try {
            resourceBundle = ResourceBundle
                    .getBundle("org.erlide.core.ErlangPluginResources");
        } catch (final MissingResourceException x) {
            x.printStackTrace();
            resourceBundle = null;
        }
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

    /**
     * Returns the string from the plugin's resource bundle, or 'key' if not
     * found.
     *
     * @param key
     *            The resource
     * @return The identified string
     */
    public static String getResourceString(final String key) {
        final ResourceBundle bundle = ErlangPlugin.getDefault()
                .getResourceBundle();
        try {
            return bundle != null ? bundle.getString(key) : key;
        } catch (final MissingResourceException e) {
            return key;
        }
    }

    /**
     * Returns the plugin's resource bundle,
     *
     * @return The requested bundle
     */
    public ResourceBundle getResourceBundle() {
        return resourceBundle;
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
            ResourcesPlugin.getWorkspace().removeSaveParticipant(this);
            ErlangCore.getModelManager().shutdown();
            platformListener.dispose();
        } finally {
            logger = null;
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
        logger = ErlLogger.init(ResourcesPlugin.getWorkspace().getRoot()
                .getLocation().toPortableString(), Platform.inDebugMode());
        ErlLogger.debug("Starting CORE " + Thread.currentThread());
        super.start(context);

        platformListener = new PlatformChangeListener();

        String dev = "";
        if (ErlideUtil.isDeveloper()) {
            dev = " erlide developer version ***";
        }
        if (ErlideUtil.isTest()) {
            dev += " test ***";
        }
        String version = getFeatureVersion();
        ErlLogger.info("*** starting Erlide v" + version + " ***" + dev);

        ErlangCore.initializeRuntimesList();

        BackendManager.getDefault().loadCodepathExtensions();

        ResourcesPlugin.getWorkspace().addSaveParticipant(this,
                new ISaveParticipant() {
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
                            (new InstanceScope()).getNode(PLUGIN_ID).flush();
                        } catch (BackingStoreException e) {
                            // ignore
                        }
                    }
                });

        ErlLogger.debug("Started CORE");
    }

    public String getFeatureVersion() {
        String version = null;
        try {
            IBundleGroupProvider[] providers = Platform
                    .getBundleGroupProviders();
            if (providers != null) {
                for (IBundleGroupProvider provider : providers) {
                    ErlLogger.debug("***: provider = %s", provider
                            .getName());
                    IBundleGroup[] bundleGroups = provider.getBundleGroups();
                    for (IBundleGroup group : bundleGroups) {
                        ErlLogger.debug("   : bundle group = %s [%s]",
                                group.getName(), group.getIdentifier());
                        if (group.getIdentifier().equals("org.erlide")) {
                            version = group.getVersion();
                            break;
                        }
                    }
                    if (version != null) {
                        break;
                    }
                }
            } else {
                ErlLogger.debug("***: no bundle group providers");
            }
        } catch (Throwable e) {
            // ignore
            e.printStackTrace();
        }
        Version coreVersion = getBundle().getVersion();
        version = version == null ? "?" : version;
        version = version + " (core=" + coreVersion.toString() + ")";
        return version;
    }

    public static void log(final IStatus status) {
        if (plugin != null) {
            Level lvl;
            switch (status.getSeverity()) {
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
            ErlLogger.log(lvl, status.getMessage());
            Throwable exception = status.getException();
            if (exception != null) {
                ErlLogger.log(lvl, exception);
            }
            plugin.getLog().log(status);
        }
    }

    public static void logErrorMessage(final String message) {
        log(new Status(IStatus.ERROR, PLUGIN_ID,
                ErlangStatusConstants.INTERNAL_ERROR, message, null));
    }

    public static void logErrorStatus(final String message, final IStatus status) {
        if (status == null) {
            logErrorMessage(message);
            return;
        }
        final MultiStatus multi = new MultiStatus(PLUGIN_ID,
                ErlangStatusConstants.INTERNAL_ERROR, message, null);
        multi.add(status);
        log(multi);
    }

    public static void log(final Throwable e) {
        log(new Status(IStatus.ERROR, PLUGIN_ID,
                ErlangStatusConstants.INTERNAL_ERROR, "Erlide internal error",
                e));
    }

    public static void log(final String msg, final Throwable thr) {
        final String id = PLUGIN_ID;
        final Status status = new Status(IStatus.ERROR, id, IStatus.OK, msg,
                thr);
        getDefault().getLog().log(status);
    }

    public static void debug(final String message) {
        if (getDefault().isDebugging()) {
            ErlLogger.debug(message);
        }
    }

}
