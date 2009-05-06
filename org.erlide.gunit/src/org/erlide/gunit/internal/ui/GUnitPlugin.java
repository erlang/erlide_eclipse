/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *   Julien Ruaux: jruaux@octo.com
 * 	 Vincent Massol: vmassol@octo.com
 *     David Saff (saff@mit.edu) - bug 102632: [JUnit] Support for JUnit 4.
 *******************************************************************************/

package org.erlide.gunit.internal.ui;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.erlide.gunit.internal.model.GUnitModel;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceReference;
import org.osgi.service.packageadmin.PackageAdmin;

/**
 * The plug-in runtime class for the JUnit plug-in.
 */
public class GUnitPlugin extends AbstractUIPlugin {

	/**
	 * The single instance of this plug-in runtime class.
	 */
	private static GUnitPlugin fgPlugin = null;

	public static final String PLUGIN_ID = "org.erlide.gunit"; //$NON-NLS-1$

	public static final String ID_EXTENSION_POINT_TESTRUN_LISTENERS = PLUGIN_ID
	+ "." + "testRunListeners"; //$NON-NLS-1$ //$NON-NLS-2$

	public static final String ID_EXTENSION_POINT_JUNIT_LAUNCHCONFIGS = PLUGIN_ID
	+ "." + "junitLaunchConfigs"; //$NON-NLS-1$ //$NON-NLS-2$

	public static final String ID_EXTENSION_POINT_TEST_KINDS = PLUGIN_ID
	+ "." + "internal_testKinds"; //$NON-NLS-1$ //$NON-NLS-2$

	public final static String TEST_SUPERCLASS_NAME = "junit.framework.TestCase"; //$NON-NLS-1$

	public final static String TEST_INTERFACE_NAME = "junit.framework.Test"; //$NON-NLS-1$

	public final static String JUNIT4_ANNOTATION_NAME = "org.junit.Test"; //$NON-NLS-1$

	public static final String SIMPLE_TEST_INTERFACE_NAME = "Test"; //$NON-NLS-1$

	/**
	 * The class path variable referring to the junit home location
	 */
	public final static String JUNIT_HOME = "JUNIT_HOME"; //$NON-NLS-1$

	/**
	 * The class path variable referring to the junit source location
	 * 
	 * @since 3.2
	 */
	public static final String JUNIT_SRC_HOME = "JUNIT_SRC_HOME"; //$NON-NLS-1$

	private static final IPath ICONS_PATH = new Path("$nl$/icons/full"); //$NON-NLS-1$

	private static final String HISTORY_DIR_NAME = "history"; //$NON-NLS-1$

	private final GUnitModel fGUnitModel = new GUnitModel();

	/**
	 * List storing the registered test run listeners
	 */
	private final ListenerList/* <TestRunListener> */fNewTestRunListeners;

	/**
	 * List storing the registered JUnit launch configuration types
	 */
	private List<String> fJUnitLaunchConfigTypeIDs;

	private BundleContext fBundleContext;

	private static boolean fIsStopped = false;

	public GUnitPlugin() {
		fgPlugin = this;
		this.fNewTestRunListeners = new ListenerList();
	}

	public static GUnitPlugin getDefault() {
		return fgPlugin;
	}

	public static Shell getActiveWorkbenchShell() {
		final IWorkbenchWindow workBenchWindow = getActiveWorkbenchWindow();
		if (workBenchWindow == null) {
			return null;
		}
		return workBenchWindow.getShell();
	}

	/**
	 * Returns the active workbench window
	 * 
	 * @return the active workbench window
	 */
	public static IWorkbenchWindow getActiveWorkbenchWindow() {
		if (fgPlugin == null) {
			return null;
		}
		final IWorkbench workBench = fgPlugin.getWorkbench();
		if (workBench == null) {
			return null;
		}
		return workBench.getActiveWorkbenchWindow();
	}

	public static IWorkbenchPage getActivePage() {
		final IWorkbenchWindow activeWorkbenchWindow = getActiveWorkbenchWindow();
		if (activeWorkbenchWindow == null) {
			return null;
		}
		return activeWorkbenchWindow.getActivePage();
	}

	public static String getPluginId() {
		return PLUGIN_ID;
	}

	public static void log(final Throwable e) {
		log(new Status(IStatus.ERROR, getPluginId(), IStatus.ERROR, "Error", e)); //$NON-NLS-1$
	}

	public static void log(final IStatus status) {
		getDefault().getLog().log(status);
	}

	public static ImageDescriptor getImageDescriptor(final String relativePath) {
		final IPath path = ICONS_PATH.append(relativePath);
		return createImageDescriptor(getDefault().getBundle(), path, true);
	}

	/**
	 * Sets the three image descriptors for enabled, disabled, and hovered to an
	 * action. The actions are retrieved from the *lcl16 folders.
	 * 
	 * @param action
	 *            the action
	 * @param iconName
	 *            the icon name
	 */
	public static void setLocalImageDescriptors(final IAction action, final String iconName) {
		setImageDescriptors(action, "lcl16", iconName); //$NON-NLS-1$
	}

	private static void setImageDescriptors(final IAction action, final String type,
			final String relPath) {
		final ImageDescriptor id = createImageDescriptor("d" + type, relPath, false); //$NON-NLS-1$
		if (id != null) {
			action.setDisabledImageDescriptor(id);
		}

		final ImageDescriptor descriptor = createImageDescriptor(
				"e" + type, relPath, true); //$NON-NLS-1$
		action.setHoverImageDescriptor(descriptor);
		action.setImageDescriptor(descriptor);
	}

	/*
	 * Creates an image descriptor for the given prefix and name in the JDT UI
	 * bundle. The path can contain variables like $NL$. If no image could be
	 * found, <code>useMissingImageDescriptor</code> decides if either the
	 * 'missing image descriptor' is returned or <code>null</code>. or
	 * <code>null</code>.
	 */
	private static ImageDescriptor createImageDescriptor(final String pathPrefix,
			final String imageName, final boolean useMissingImageDescriptor) {
		final IPath path = ICONS_PATH.append(pathPrefix).append(imageName);
		return createImageDescriptor(GUnitPlugin.getDefault().getBundle(),
				path, useMissingImageDescriptor);
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
	private static ImageDescriptor createImageDescriptor(final Bundle bundle,
			final IPath path, final boolean useMissingImageDescriptor) {
		final URL url = FileLocator.find(bundle, path, null);
		if (url != null) {
			return ImageDescriptor.createFromURL(url);
		}
		if (useMissingImageDescriptor) {
			return ImageDescriptor.getMissingImageDescriptor();
		}
		return null;
	}

	/**
	 * @see AbstractUIPlugin#start(BundleContext)
	 */
	@Override
	public void start(final BundleContext context) throws Exception {
		super.start(context);
		this.fBundleContext = context;
		this.fGUnitModel.start();
	}

	/**
	 * @see AbstractUIPlugin#stop(BundleContext)
	 */
	@Override
	public void stop(final BundleContext context) throws Exception {
		fIsStopped = true;
		try {
			this.fGUnitModel.stop();
		} finally {
			super.stop(context);
		}
		this.fBundleContext = null;
	}

	public static GUnitModel getModel() {
		return getDefault().fGUnitModel;
	}

	/**
	 * Loads the registered JUnit launch configurations
	 */
	private void loadLaunchConfigTypeIDs() {
		this.fJUnitLaunchConfigTypeIDs = new ArrayList<String>();
		final IExtensionPoint extensionPoint = Platform.getExtensionRegistry()
		.getExtensionPoint(ID_EXTENSION_POINT_JUNIT_LAUNCHCONFIGS);
		if (extensionPoint == null) {
			return;
		}
		final IConfigurationElement[] configs = extensionPoint
		.getConfigurationElements();

		for (int i = 0; i < configs.length; i++) {
			final String configTypeID = configs[i].getAttribute("configTypeID"); //$NON-NLS-1$
			this.fJUnitLaunchConfigTypeIDs.add(configTypeID);
		}
	}

	/**
	 * @return a list of all JUnit launch configuration types
	 */
	public List/* <String> */<String> getJUnitLaunchConfigTypeIDs() {
		if (this.fJUnitLaunchConfigTypeIDs == null) {
			loadLaunchConfigTypeIDs();
		}
		return this.fJUnitLaunchConfigTypeIDs;
	}

	/**
	 * Returns the bundle for a given bundle name, regardless whether the bundle
	 * is resolved or not.
	 * 
	 * @param bundleName
	 *            the bundle name
	 * @return the bundle
	 * @since 3.2
	 */
	public Bundle getBundle(final String bundleName) {
		final Bundle[] bundles = getBundles(bundleName, null);
		if (bundles != null && bundles.length > 0) {
			return bundles[0];
		}
		return null;
	}

	/**
	 * Returns the bundles for a given bundle name,
	 * 
	 * @param bundleName
	 *            the bundle name
	 * @return the bundles of the given name
	 */
	public Bundle[] getBundles(final String bundleName, final String version) {
		Bundle[] bundles = Platform.getBundles(bundleName, version);
		if (bundles != null) {
			return bundles;
		}

		// Accessing unresolved bundle
		final ServiceReference serviceRef = this.fBundleContext
		.getServiceReference(PackageAdmin.class.getName());
		final PackageAdmin admin = (PackageAdmin) this.fBundleContext
		.getService(serviceRef);
		bundles = admin.getBundles(bundleName, version);
		if (bundles != null && bundles.length > 0) {
			return bundles;
		}
		return null;
	}

	/**
	 * @return a <code>ListenerList</code> of all <code>TestRunListener</code>s
	 */
	public ListenerList/* <TestRunListener> */getNewTestRunListeners() {
		return this.fNewTestRunListeners;
	}

	public static boolean isStopped() {
		return fIsStopped;
	}

	public IDialogSettings getDialogSettingsSection(final String name) {
		final IDialogSettings dialogSettings = getDialogSettings();
		IDialogSettings section = dialogSettings.getSection(name);
		if (section == null) {
			section = dialogSettings.addNewSection(name);
		}
		return section;
	}

	public static File getHistoryDirectory() throws IllegalStateException {
		final File historyDir = getDefault().getStateLocation().append(
				HISTORY_DIR_NAME).toFile();
		if (!historyDir.isDirectory()) {
			historyDir.mkdir();
		}
		return historyDir;
	}

}
