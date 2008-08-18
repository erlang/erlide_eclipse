/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime;

import java.io.IOException;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.logging.ConsoleHandler;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Logger;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.erlide.basiccore.ErlLogger;
import org.erlide.jinterface.ICodeBundle;
import org.erlide.jinterface.InterfacePlugin;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.IdeBackend;
import org.osgi.framework.BundleContext;

import erlang.ErlideBackend;

/**
 * The main plugin class to be used in the desktop.
 */
public class ErlangLaunchPlugin extends Plugin implements ICodeBundle {

	public static final String PLUGIN_ID = "org.erlide.launching";

	// The shared instance.
	private static ErlangLaunchPlugin plugin;

	/**
	 * Resource bundle.
	 */
	private ResourceBundle resourceBundle;

	/**
	 * The constructor.
	 */
	public ErlangLaunchPlugin() {
		plugin = this;
		try {
			resourceBundle = ResourceBundle
					.getBundle("org.erlide.runtime.ErlangLaunchPluginResources");
		} catch (final MissingResourceException x) {
			x.printStackTrace();
			resourceBundle = null;
		}

		if (BackendManager.isDeveloper()) {
			// new EpmdWatchJob().schedule();
		}

	}

	/**
	 * This method is called upon plug-in activation
	 */
	@Override
	public void start(final BundleContext context) throws Exception {
		super.start(context);

		Handler fh;
		try {
			final ErlLogger.ErlSimpleFormatter erlSimpleFormatter = new ErlLogger.ErlSimpleFormatter();
			final Logger logger = Logger.getLogger("org.erlide");

			String dir = ResourcesPlugin.getWorkspace().getRoot().getLocation()
					.toPortableString();
			dir = dir == null ? "c:/" : dir;
			fh = new FileHandler(dir + "_erlide.log");
			fh.setFormatter(erlSimpleFormatter);
			logger.addHandler(fh);

			final ConsoleHandler consoleHandler = new ConsoleHandler();
			consoleHandler.setFormatter(erlSimpleFormatter);
			consoleHandler.setLevel(java.util.logging.Level.FINEST);
			logger.addHandler(consoleHandler);

			logger.setLevel(java.util.logging.Level.FINEST);
		} catch (SecurityException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		ErlLogger.debug("Starting LAUNCHING " + Thread.currentThread());

		String dev = "";
		if (BackendManager.isDeveloper()) {
			dev = " erlide developer version ***";
		}
		if (BackendManager.isTest()) {
			dev += " test ***";
		}
		ErlLogger
				.info("*** starting Erlide v"
						+ getBundle().getHeaders().get("Bundle-Version")
						+ " ***" + dev);

		BackendManager.getDefault().register(InterfacePlugin.getDefault());
		BackendManager.getDefault().register(this);

		IdeBackend b = BackendManager.getDefault().getIdeBackend();
		ErlideBackend.init(b, BackendManager
				.buildNodeName(BackendManager.JAVA_NODE_LABEL));
		ErlLogger.debug("Started LAUNCHING");
	}

	/**
	 * This method is called when the plug-in is stopped
	 */
	@Override
	public void stop(final BundleContext context) throws Exception {
		BackendManager.getDefault().removePlugin(this);

		super.stop(context);
		plugin = null;
	}

	/**
	 * Returns the shared instance.
	 */
	public static ErlangLaunchPlugin getDefault() {
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
		final ResourceBundle bundle = ErlangLaunchPlugin.getDefault()
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

	/**
	 * Logs the specified status with this plug-in's log.
	 * 
	 * @param status
	 *            status to log
	 */
	public static void log(final IStatus status) {
		getDefault().getLog().log(status);
	}

	/**
	 * Logs an internal error with the specified message.
	 * 
	 * @param message
	 *            the error message to log
	 */
	public static void logErrorMessage(final String message) {
		log(new Status(IStatus.ERROR, PLUGIN_ID, 150, message, null));
	}

	/**
	 * Logs an internal error with the specified throwable
	 * 
	 * @param e
	 *            the exception to be logged
	 */
	public static void log(final Throwable e) {
		log(new Status(IStatus.ERROR, PLUGIN_ID, 150, "internal error", e));
	}

	public static void log(final String message, final Exception e) {
		log(new Status(IStatus.ERROR, PLUGIN_ID, 150, message, e));
	}

	public static void debug(final String message) {
		if (getDefault().isDebugging()) {
			ErlLogger.debug(message);
		}
	}

	public void start() {
		// ErlideBackend.init(ideBackend);
	}

}
