package org.erlide.gunit;

import java.net.URL;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.erlide.gunit.model.GUnitModel;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle
 */
public class GUnitPlugin extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "org.erlide.testing.framework";

	private static final IPath ICONS_PATH = new Path("$nl$/icons");

	// The shared instance
	private static GUnitPlugin plugin;

	private final GUnitModel fBTErlModel = new GUnitModel();

	/**
	 * The constructor
	 */
	public GUnitPlugin() {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext
	 * )
	 */
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		// fBTErlModel.start();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext
	 * )
	 */
	@Override
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		// try {
		// fBTErlModel.stop();
		// } finally {
		// super.stop(context);
		// }
	}

	public static IWorkbenchWindow getActiveWorkbenchWindow() {
		if (plugin == null) {
			return null;
		}
		IWorkbench workBench = plugin.getWorkbench();
		if (workBench == null) {
			return null;
		}
		return workBench.getActiveWorkbenchWindow();
	}

	public static IWorkbenchPage getActivePage() {
		IWorkbenchWindow activeWorkbenchWindow = getActiveWorkbenchWindow();
		if (activeWorkbenchWindow == null) {
			return null;
		}
		return activeWorkbenchWindow.getActivePage();
	}

	public static GUnitModel getModel() {
		return getDefault().fBTErlModel;
	}

	/**
	 * Returns the shared instance
	 * 
	 * @return the shared instance
	 */
	public static GUnitPlugin getDefault() {
		return plugin;
	}

	/* COPIED FROM JUNIT PLUGIN */
	/*******************************************************************************
	 * Copyright (c) 2000, 2007 IBM Corporation and others. All rights reserved.
	 * This program and the accompanying materials are made available under the
	 * terms of the Eclipse Public License v1.0 which accompanies this
	 * distribution, and is available at
	 * http://www.eclipse.org/legal/epl-v10.html
	 * 
	 * Contributors: IBM Corporation - initial API and implementation Julien
	 * Ruaux: jruaux@octo.com Vincent Massol: vmassol@octo.com David Saff
	 * (saff@mit.edu) - bug 102632: [JUnit] Support for JUnit 4.
	 *******************************************************************************/

	public static ImageDescriptor getImageDescriptor(String relativePath) {
		IPath path = ICONS_PATH.append(relativePath);
		return createImageDescriptor(getDefault().getBundle(), path, true);
	}

	/**
	 * Creates an image descriptor for the given path in a bundle. The path can
	 * contain variables like $NL$. If no image could be found,
	 * <code>useMissingImageDescriptor</code> decides if either the 'missing
	 * image descriptor' is returned or <code>null</code>.
	 * 
	 * @param bundle
	 * @param path
	 * @param useMissingImageDescriptor
	 * @return an {@link ImageDescriptor}, or <code>null</code> iff there's no
	 *         image at the given location and
	 *         <code>useMissingImageDescriptor</code> is <code>true</code>
	 */
	private static ImageDescriptor createImageDescriptor(Bundle bundle,
			IPath path, boolean useMissingImageDescriptor) {
		URL url = FileLocator.find(bundle, path, null);
		if (url != null) {
			return ImageDescriptor.createFromURL(url);
		}
		if (useMissingImageDescriptor) {
			return ImageDescriptor.getMissingImageDescriptor();
		}
		return null;
	}
}
