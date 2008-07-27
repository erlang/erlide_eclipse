/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.erlide.basiccore.ErlLogger;
import org.erlide.basicui.util.IErlangStatusConstants;
import org.erlide.basicui.util.ImageDescriptorRegistry;
import org.erlide.core.ErlangPlugin;
import org.erlide.jinterface.ICodeBundle;
import org.erlide.jinterface.rpc.RpcUtil;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.ui.internal.folding.ErlangFoldingStructureProviderRegistry;
import org.erlide.ui.util.BackendManagerPopup;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 * 
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class ErlideUIPlugin extends AbstractUIPlugin implements ICodeBundle {

	/**
	 * The plugin id
	 */
	public static final String PLUGIN_ID = "org.erlide.ui";

	/**
	 * The shared instance.
	 */
	private static ErlideUIPlugin plugin;

	/**
	 * Resource bundle.
	 */
	private ResourceBundle resourceBundle;

	private ImageDescriptorRegistry fImageDescriptorRegistry;

	/**
	 * The extension point registry for the
	 * <code>org.eclipse.jdt.ui.javaFoldingStructureProvider</code> extension
	 * point.
	 * 
	 * @since 3.0
	 */
	private ErlangFoldingStructureProviderRegistry fFoldingStructureProviderRegistry;

	/**
	 * The constructor.
	 */
	public ErlideUIPlugin() {
		super();
		plugin = this;

		try {
			resourceBundle = ResourceBundle
					.getBundle("org.erlide.ui.ErlideUIPluginResources");
		} catch (final MissingResourceException x) {
			x.printStackTrace();
			resourceBundle = null;
		}

	}

	/**
	 * This method is called upon plug-in activation
	 * 
	 * @param context
	 *            The context
	 * @throws Exception
	 *             if a problem occurs
	 */
	@Override
	public void start(BundleContext context) throws Exception {
		ErlLogger.debug("Starting UI " + Thread.currentThread());
		super.start(context);

		// set this classloader to be used with erlang rpc
		RpcUtil.loader = getClass().getClassLoader();

		// we must ensure this
		BackendManager.getDefault().register(ErlangPlugin.getDefault());
		BackendManager.getDefault().register(this);

		new InitializeAfterLoadJob().schedule();

		if (BackendManager.isDeveloper()) {
			BackendManagerPopup.init();
		}
		ErlLogger.debug("Started UI");
	}

	/**
	 * This method is called when the plug-in is stopped
	 * 
	 * @param context
	 *            the context
	 * @throws Exception
	 *             if a problem occurs
	 */
	@Override
	public void stop(BundleContext context) throws Exception {
		BackendManager.getDefault().removePlugin(this);

		super.stop(context);
	}

	/**
	 * Returns the shared instance.
	 * 
	 * @return The plugin
	 */
	public static ErlideUIPlugin getDefault() {
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
	public static String getResourceString(String key) {
		final ResourceBundle bundle = ErlideUIPlugin.getDefault()
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

	/**
	 * Returns the standard display to be used. The method first checks, if the
	 * thread calling this method has an associated display. If so, this display
	 * is returned. Otherwise the method returns the default display.
	 * 
	 * @return the standard display
	 */
	public static Display getStandardDisplay() {
		Display display = Display.getCurrent();
		if (display == null) {
			display = Display.getDefault();
		}

		return display;
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
			url = new URL(baseURL, IErlideUIConstants.ICON_PATH + id);
		} catch (final MalformedURLException e) {
			// ignore exception
		}

		getImageRegistry().put(id, ImageDescriptor.createFromURL(url));
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
	 * Returns the image for the given image PLUGIN_ID. Returns null if there is
	 * no such image.
	 * 
	 * @param id
	 *            The image id
	 * 
	 * @return The image
	 */
	public Image getImage(String id) {
		final Image returnImage = getImageRegistry().get(id);
		return returnImage;
	}

	/**
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#initializeImageRegistry(org.eclipse.jface.resource.ImageRegistry)
	 */
	@Override
	protected void initializeImageRegistry(ImageRegistry reg) {
		super.initializeImageRegistry(reg);

		final URL baseURL = getBundle().getEntry("/");

		createImageDescriptor(IErlideUIConstants.IMG_CONSOLE, baseURL);
		createImageDescriptor(IErlideUIConstants.IMG_NEW_PROJECT_WIZARD,
				baseURL);
		createImageDescriptor(IErlideUIConstants.IMG_PROJECT_LABEL, baseURL);
		createImageDescriptor(IErlideUIConstants.IMG_PACKAGE_FOLDER_LABEL,
				baseURL);
		createImageDescriptor(IErlideUIConstants.IMG_PACKAGE_LABEL, baseURL);
		createImageDescriptor(IErlideUIConstants.IMG_FILE_LABEL, baseURL);
		createImageDescriptor(IErlideUIConstants.IMG_FOLDER_LABLE, baseURL);
		createImageDescriptor(IErlideUIConstants.IMG_DISABLED_REFRESH, baseURL);
		createImageDescriptor(IErlideUIConstants.IMG_REFRESH, baseURL);
		createImageDescriptor(IErlideUIConstants.IMG_DISABLED_IMPORT, baseURL);
		createImageDescriptor(IErlideUIConstants.IMG_IMPORT, baseURL);
		createImageDescriptor(IErlideUIConstants.IMG_DISABLED_EXPORT, baseURL);
		createImageDescriptor(IErlideUIConstants.IMG_EXPORT, baseURL);
		createImageDescriptor(IErlideUIConstants.IMG_COLLAPSEALL, baseURL);
		createImageDescriptor(IErlideUIConstants.IMG_PROJECT_CLOSED_LABEL,
				baseURL);
	}

	/**
	 * @return
	 */
	public static IWorkbenchPage getActivePage() {
		final IWorkbenchWindow w = getActiveWorkbenchWindow();
		if (w != null) {
			return w.getActivePage();
		}
		return null;
	}

	public static IWorkbenchWindow getActiveWorkbenchWindow() {
		return getDefault().getWorkbench().getActiveWorkbenchWindow();
	}

	/**
	 * Returns the active workbench shell or <code>null</code> if none
	 * 
	 * @return the active workbench shell or <code>null</code> if none
	 */
	public static Shell getActiveWorkbenchShell() {
		final IWorkbenchWindow window = getActiveWorkbenchWindow();
		if (window != null) {
			return window.getShell();
		}
		return null;
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

	/**
	 * Returns the registry of the extensions to the
	 * <code>org.erlide.ui.erlangFoldingStructureProvider</code> extension
	 * point.
	 * 
	 * @return the registry of contributed
	 *         <code>IErlangFoldingStructureProvider</code>
	 * @since 3.0
	 */
	public synchronized ErlangFoldingStructureProviderRegistry getFoldingStructureProviderRegistry() {
		if (fFoldingStructureProviderRegistry == null) {
			fFoldingStructureProviderRegistry = new ErlangFoldingStructureProviderRegistry();
		}
		return fFoldingStructureProviderRegistry;
	}

	public static void debug(String message) {
		if (getDefault().isDebugging()) {
			ErlLogger.debug(message);
		}
	}

	public void start() {
		// TODO Auto-generated method stub

	}

}
