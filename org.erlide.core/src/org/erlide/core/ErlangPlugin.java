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
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.util.ErlideUtil;
import org.erlide.core.platform.PlatformChangeListener;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.runtime.backend.BackendManager;
import org.osgi.framework.Bundle;
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

	/**
	 * The plugin id
	 */
	public static final String PLUGIN_ID = "org.erlide.core";

	/**
	 * The builder id
	 */
	public static final String BUILDER_ID = PLUGIN_ID + ".erlbuilder";

	/**
	 * the nature id
	 */
	public static final String NATURE_ID = PLUGIN_ID + ".erlnature";

	private static final boolean TOUCH_ALL_ERLANG_PROJECTS_ON_LAUNCH = false;

	/**
	 * The shared instance.
	 */
	private static ErlangPlugin plugin;

	/**
	 * Resource bundle.
	 */
	private ResourceBundle resourceBundle;

	private Logger logger;

	private PlatformChangeListener platformListener;

	/**
	 * The constructor.
	 */
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
		String version;
		Bundle feature = Platform.getBundle("org.erlide");
		// TODO this is null, why?
		if (feature == null) {
			// when running launched from eclipse (testing), features aren't
			// available
			feature = getBundle();
			version = "(" + (String) feature.getHeaders().get("Bundle-Version")
					+ ")";
		} else {
			// TODO read from feature.xml
			version = (String) feature.getHeaders().get("Bundle-Version");
		}
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
						savePluginPreferences();
					}
				});

		ErlLogger.debug("Started CORE");
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

	public static void initializeAfterLoad(final IProgressMonitor monitor) {
		try {
			if (TOUCH_ALL_ERLANG_PROJECTS_ON_LAUNCH) {
				final IWorkspaceRunnable runnable = new IWorkspaceRunnable() {

					public void run(final IProgressMonitor progressMonitor)
							throws CoreException {
						IProject[] projects;
						// projects = model.getJavaProjects();
						final IWorkspace root = ResourcesPlugin.getWorkspace();
						projects = root.getRoot().getProjects();

						if (projects != null) {
							for (final IProject project : projects) {
								try {
									if (project
											.hasNature(ErlangPlugin.NATURE_ID)) {
										project.touch(progressMonitor);
									}
								} catch (final CoreException e) {
									// could not touch this project: ignore
								}
							}
						}
					}
				};

				final long millis0 = System.currentTimeMillis();
				ResourcesPlugin.getWorkspace().run(runnable, monitor);
				final long millis = System.currentTimeMillis() - millis0;
				ErlLogger.debug("Time for touch : " + millis);
			}
		} catch (final CoreException e) {
			// could not touch all projects
		}

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
