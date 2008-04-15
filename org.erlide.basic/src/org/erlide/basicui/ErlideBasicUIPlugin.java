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
package org.erlide.basicui;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.logging.ConsoleHandler;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Logger;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.erlide.basiccore.ErlLogger;
import org.erlide.basiccore.ErtsPreferences;
import org.erlide.basicui.util.IErlangStatusConstants;
import org.erlide.basicui.util.ImageDescriptorRegistry;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleException;

/**
 * The main plugin class to be used in the desktop.
 */
public class ErlideBasicUIPlugin extends AbstractUIPlugin {

	/**
	 * The plugin id
	 */
	public static final String PLUGIN_ID = "org.erlide.basicui";

	// The shared instance.
	private static ErlideBasicUIPlugin plugin;

	private ResourceBundle resourceBundle;

	/**
	 * The constructor.
	 */
	public ErlideBasicUIPlugin() {
		super();
		plugin = this;

		try {
			resourceBundle = ResourceBundle.getBundle(PLUGIN_ID
					+ ".ErlideBasicUIPluginResources");
		} catch (final MissingResourceException x) {
			x.printStackTrace();
			resourceBundle = null;
		}

	}

	private ErtsPreferences preferences;

	private ImageDescriptorRegistry fImageDescriptorRegistry;

	private Bundle fLaunchBundle;

	/**
	 * @return
	 */
	public ErtsPreferences getPreferences() {
		if (preferences == null) {
			preferences = new ErtsPreferences();
		}
		return preferences;
	}

	/**
	 * This method is called upon plug-in activation
	 */
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);

		Handler fh;
		try {
			final ErlLogger.ErlSimpleFormatter erlSimpleFormatter = new ErlLogger.ErlSimpleFormatter();
			final Logger logger = Logger.getLogger("org.erlide");

			String dir = ResourcesPlugin.getWorkspace().getRoot().getLocation()
					.toPortableString();
			dir = dir == null ? "c:/" : dir;
			fh = new FileHandler(dir + "erlide.log");
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

	}

	/**
	 * This method is called when the plug-in is stopped
	 */
	@Override
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
		plugin = null;
	}

	/**
	 * Returns the shared instance.
	 */
	public static ErlideBasicUIPlugin getDefault() {
		return plugin;
	}

	/**
	 * Returns the image descriptor for the given image PLUGIN_ID. Returns null
	 * if there is no such image.
	 * 
	 * @param id
	 *            The image id
	 * 
	 * @return The image descriptor
	 */
	public ImageDescriptor getImageDescriptor(String id) {
		final ImageDescriptor returnImageDescriptor = getImageRegistry()
				.getDescriptor(id);
		return returnImageDescriptor;
	}

	/**
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#initializeImageRegistry(org.eclipse.jface.resource.ImageRegistry)
	 */
	@Override
	protected void initializeImageRegistry(ImageRegistry reg) {
		super.initializeImageRegistry(reg);

		final URL baseURL = getBundle().getEntry("/");

		createImageDescriptor(IErlideBasicUIConstants.IMG_ERLANG_LOGO, baseURL);
	}

	/**
	 * Creates an image and places it in the image registry.
	 * 
	 * @param id
	 *            The image id
	 * @param baseURL
	 *            The descriptor url
	 */
	protected void createImageDescriptor(String id, URL baseURL) {
		URL url = null;
		try {
			url = new URL(baseURL, IErlideBasicUIConstants.ICON_PATH + id);
		} catch (final MalformedURLException e) {
			// ignore exception
		}

		getImageRegistry().put(id, ImageDescriptor.createFromURL(url));
	}

	/**
	 * Returns the string from the plugin's resource bundle, or 'key' if not
	 * found.
	 * 
	 * @param key
	 *            The resource
	 * @return The identified string
	 */
	public static String getResourceString(String key) {
		final ResourceBundle bundle = ErlideBasicUIPlugin.getDefault()
				.getResourceBundle();
		try {

			final String returnString = (bundle != null) ? bundle
					.getString(key) : key;
			return returnString;
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

	public static void log(Exception e) {
		log(new Status(IStatus.ERROR, PLUGIN_ID,
				IErlangStatusConstants.INTERNAL_ERROR, e.getMessage(), null));
	}

	public static void log(IStatus status) {
		getDefault().getLog().log(status);
	}

	public static void logErrorMessage(String message) {
		log(new Status(IStatus.ERROR, PLUGIN_ID,
				IErlangStatusConstants.INTERNAL_ERROR, message, null));
	}

	public static void logErrorStatus(String message, IStatus status) {
		if (status == null) {
			logErrorMessage(message);
			return;
		}
		final MultiStatus multi = new MultiStatus(PLUGIN_ID,
				IErlangStatusConstants.INTERNAL_ERROR, message, null);
		multi.add(status);
		log(multi);
	}

	public static void log(Throwable e) {
		log(new Status(IStatus.ERROR, PLUGIN_ID,
				IErlangStatusConstants.INTERNAL_ERROR, "Erlide internal error",
				e));
	}

	public static ImageDescriptorRegistry getImageDescriptorRegistry() {
		return getDefault().internalGetImageDescriptorRegistry();
	}

	private synchronized ImageDescriptorRegistry internalGetImageDescriptorRegistry() {
		if (fImageDescriptorRegistry == null) {
			fImageDescriptorRegistry = new ImageDescriptorRegistry();
		}
		return fImageDescriptorRegistry;
	}

	public void setLaunchBundle(Bundle b) {
		fLaunchBundle = b;
	}

	// TODO can't we restart the backend another way?
	public void restartBundle() {
		if (fLaunchBundle != null) {
			try {
				// Stopping handler
				fLaunchBundle.stop();
				while (fLaunchBundle.getState() != Bundle.RESOLVED) {
					Thread.sleep(2000);
				}

				// starting handler
				fLaunchBundle.start();
				while (fLaunchBundle.getState() != Bundle.ACTIVE) {
					Thread.sleep(2000);
				}
			} catch (BundleException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
}
