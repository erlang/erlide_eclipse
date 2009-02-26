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
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.MultiStatus;
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

	private final GUnitModel fJUnitModel = new GUnitModel();

	/**
	 * List storing the registered test run listeners
	 */
	private List/* <ITestRunListener> */fLegacyTestRunListeners;

	/**
	 * List storing the registered test run listeners
	 */
	private ListenerList/* <TestRunListener> */fNewTestRunListeners;

	/**
	 * List storing the registered JUnit launch configuration types
	 */
	private List fJUnitLaunchConfigTypeIDs;

	private BundleContext fBundleContext;

	private static boolean fIsStopped = false;

	public GUnitPlugin() {
		fgPlugin = this;
		fNewTestRunListeners = new ListenerList();
	}

	public static GUnitPlugin getDefault() {
		return fgPlugin;
	}

	public static Shell getActiveWorkbenchShell() {
		IWorkbenchWindow workBenchWindow = getActiveWorkbenchWindow();
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
		IWorkbench workBench = fgPlugin.getWorkbench();
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

	public static String getPluginId() {
		return PLUGIN_ID;
	}

	public static void log(Throwable e) {
		log(new Status(IStatus.ERROR, getPluginId(), IStatus.ERROR, "Error", e)); //$NON-NLS-1$
	}

	public static void log(IStatus status) {
		getDefault().getLog().log(status);
	}

	public static ImageDescriptor getImageDescriptor(String relativePath) {
		IPath path = ICONS_PATH.append(relativePath);
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
	public static void setLocalImageDescriptors(IAction action, String iconName) {
		setImageDescriptors(action, "lcl16", iconName); //$NON-NLS-1$
	}

	private static void setImageDescriptors(IAction action, String type,
			String relPath) {
		ImageDescriptor id = createImageDescriptor("d" + type, relPath, false); //$NON-NLS-1$
		if (id != null) {
			action.setDisabledImageDescriptor(id);
		}

		ImageDescriptor descriptor = createImageDescriptor(
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
	private static ImageDescriptor createImageDescriptor(String pathPrefix,
			String imageName, boolean useMissingImageDescriptor) {
		IPath path = ICONS_PATH.append(pathPrefix).append(imageName);
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

	/**
	 * @see AbstractUIPlugin#start(BundleContext)
	 */
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		fBundleContext = context;
		fJUnitModel.start();
	}

	/**
	 * @see AbstractUIPlugin#stop(BundleContext)
	 */
	@Override
	public void stop(BundleContext context) throws Exception {
		fIsStopped = true;
		try {
			fJUnitModel.stop();
		} finally {
			super.stop(context);
		}
		fBundleContext = null;
	}

	public static GUnitModel getModel() {
		return getDefault().fJUnitModel;
	}

	/**
	 * Initializes TestRun Listener extensions
	 * 
	 * @deprecated
	 */
	@Deprecated
	private void loadTestRunListeners() {
		fLegacyTestRunListeners = new ArrayList();
		IExtensionPoint extensionPoint = Platform.getExtensionRegistry()
				.getExtensionPoint(ID_EXTENSION_POINT_TESTRUN_LISTENERS);
		if (extensionPoint == null) {
			return;
		}
		IConfigurationElement[] configs = extensionPoint
				.getConfigurationElements();
		MultiStatus status = new MultiStatus(PLUGIN_ID, IStatus.OK,
				"Could not load some testRunner extension points", null); //$NON-NLS-1$ 	

		for (int i = 0; i < configs.length; i++) {
			try {
				Object testRunListener = configs[i]
						.createExecutableExtension("class"); //$NON-NLS-1$
				if (testRunListener instanceof org.erlide.gunit.ITestRunListener) {
					fLegacyTestRunListeners.add(testRunListener);
				}
			} catch (CoreException e) {
				status.add(e.getStatus());
			}
		}
		if (!status.isOK()) {
			GUnitPlugin.log(status);
		}
	}

	/**
	 * Loads the registered JUnit launch configurations
	 */
	private void loadLaunchConfigTypeIDs() {
		fJUnitLaunchConfigTypeIDs = new ArrayList();
		IExtensionPoint extensionPoint = Platform.getExtensionRegistry()
				.getExtensionPoint(ID_EXTENSION_POINT_JUNIT_LAUNCHCONFIGS);
		if (extensionPoint == null) {
			return;
		}
		IConfigurationElement[] configs = extensionPoint
				.getConfigurationElements();

		for (int i = 0; i < configs.length; i++) {
			String configTypeID = configs[i].getAttribute("configTypeID"); //$NON-NLS-1$
			fJUnitLaunchConfigTypeIDs.add(configTypeID);
		}
	}

	/**
	 * @return an array of all TestRun listeners
	 * @deprecated
	 */
	@Deprecated
	public org.erlide.gunit.ITestRunListener[] getTestRunListeners() {
		if (fLegacyTestRunListeners == null) {
			loadTestRunListeners();
		}
		return (org.erlide.gunit.ITestRunListener[]) fLegacyTestRunListeners
				.toArray(new org.erlide.gunit.ITestRunListener[fLegacyTestRunListeners
						.size()]);
	}

	/**
	 * @return a list of all JUnit launch configuration types
	 */
	public List/* <String> */getJUnitLaunchConfigTypeIDs() {
		if (fJUnitLaunchConfigTypeIDs == null) {
			loadLaunchConfigTypeIDs();
		}
		return fJUnitLaunchConfigTypeIDs;
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
	public Bundle getBundle(String bundleName) {
		Bundle[] bundles = getBundles(bundleName, null);
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
	public Bundle[] getBundles(String bundleName, String version) {
		Bundle[] bundles = Platform.getBundles(bundleName, version);
		if (bundles != null) {
			return bundles;
		}

		// Accessing unresolved bundle
		ServiceReference serviceRef = fBundleContext
				.getServiceReference(PackageAdmin.class.getName());
		PackageAdmin admin = (PackageAdmin) fBundleContext
				.getService(serviceRef);
		bundles = admin.getBundles(bundleName, version);
		if (bundles != null && bundles.length > 0) {
			return bundles;
		}
		return null;
	}

	/**
	 * Adds a TestRun listener to the collection of listeners
	 * 
	 * @param newListener
	 *            the listener to add
	 * @deprecated
	 */
	@Deprecated
	public void addTestRunListener(org.erlide.gunit.ITestRunListener newListener) {
		if (fLegacyTestRunListeners == null) {
			loadTestRunListeners();
		}

		for (Iterator iter = fLegacyTestRunListeners.iterator(); iter.hasNext();) {
			Object o = iter.next();
			if (o == newListener) {
				return;
			}
		}
		fLegacyTestRunListeners.add(newListener);
	}

	/**
	 * Removes a TestRun listener to the collection of listeners
	 * 
	 * @param newListener
	 *            the listener to remove
	 * @deprecated
	 */
	@Deprecated
	public void removeTestRunListener(
			org.erlide.gunit.ITestRunListener newListener) {
		if (fLegacyTestRunListeners != null) {
			fLegacyTestRunListeners.remove(newListener);
		}
	}

	/**
	 * @return a <code>ListenerList</code> of all <code>TestRunListener</code>s
	 */
	public ListenerList/* <TestRunListener> */getNewTestRunListeners() {
		return fNewTestRunListeners;
	}

	public static boolean isStopped() {
		return fIsStopped;
	}

	public IDialogSettings getDialogSettingsSection(String name) {
		IDialogSettings dialogSettings = getDialogSettings();
		IDialogSettings section = dialogSettings.getSection(name);
		if (section == null) {
			section = dialogSettings.addNewSection(name);
		}
		return section;
	}

	public static File getHistoryDirectory() throws IllegalStateException {
		File historyDir = getDefault().getStateLocation().append(
				HISTORY_DIR_NAME).toFile();
		if (!historyDir.isDirectory()) {
			historyDir.mkdir();
		}
		return historyDir;
	}

}
