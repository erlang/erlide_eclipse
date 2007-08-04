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

import java.net.MalformedURLException;
import java.net.URL;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.preference.IPreferenceNode;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.preference.PreferenceManager;
import org.eclipse.jface.preference.PreferenceNode;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.erlide.basiccore.ErtsPreferences;
import org.erlide.basicui.prefs.ErtsPreferencePage;
import org.erlide.basicui.util.IErlangStatusConstants;
import org.erlide.basicui.util.ImageDescriptorRegistry;
import org.osgi.framework.BundleContext;

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
			resourceBundle = ResourceBundle.getBundle(PLUGIN_ID +
					".ErlideBasicUIPluginResources");
		} catch (final MissingResourceException x) {
			x.printStackTrace();
			resourceBundle = null;
		}

	}

	private ErtsPreferences preferences;

	private ImageDescriptorRegistry fImageDescriptorRegistry;

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

	// this is synchronous: we don't want anything to proceed before erlang path
	// is fixed!
	public static void showErtsPreferencesDialog(int tries) {
		try {
			final IWorkbench workbench = PlatformUI.getWorkbench();
			final IWorkbenchWindow window = workbench
					.getActiveWorkbenchWindow();
			final Shell shell = window.getShell();

			final IPreferenceNode targetNode = new PreferenceNode(
					ErtsPreferencePage.ID + ".2", new ErtsPreferencePage());
			final PreferenceManager manager = new PreferenceManager();
			manager.addToRoot(targetNode);

			final PreferenceDialog dialog = new PreferenceDialog(shell, manager);
			dialog.create();
			dialog.setMessage("Please configure your Erlang installation! (" +
					tries + " more tries");
			dialog.open();
		} catch (final Exception e) {
			System.out
					.println("Couldn't show preference dialog, and Erlang isn't configured...");
		}
		;
	}

	public static void closeWorkbench() {
		final IWorkbench workbench = PlatformUI.getWorkbench();
		final IWorkbenchWindow window = workbench.getActiveWorkbenchWindow();
		final Shell shell = window.getShell();
		final MessageBox box = new MessageBox(shell, SWT.OK | SWT.ICON_ERROR |
				SWT.APPLICATION_MODAL);
		box.setMessage("Could not find an Erlang installation. Giving up...");
		box.setText("Fatal Error");
		box.open();
		// TODO what do we do?
		workbench.close();
	}
}
