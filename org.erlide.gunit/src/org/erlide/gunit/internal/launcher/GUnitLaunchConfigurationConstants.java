/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.erlide.gunit.internal.launcher;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.erlide.core.erlang.IErlProject;
import org.erlide.gunit.internal.ui.GUnitPlugin;

/**
 * Attribute keys used by the IJUnitLaunchConfiguration. Note that these
 * constants are not API and might change in the future.
 */
public class GUnitLaunchConfigurationConstants {

	public static final String MODE_RUN_QUIETLY_MODE = "runQuietly"; //$NON-NLS-1$

	public static final String ID_JUNIT_APPLICATION = "org.erlide.gunit.launchconfig"; //$NON-NLS-1$

	public static final String ATTR_NO_DISPLAY = GUnitPlugin.PLUGIN_ID
			+ ".NO_DISPLAY"; //$NON-NLS-1$

	public static final String ATTR_PORT = GUnitPlugin.PLUGIN_ID + ".PORT"; //$NON-NLS-1$

	/**
	 * The test method, or "" iff running the whole test type.
	 */
	public static final String ATTR_TEST_METHOD_NAME = GUnitPlugin.PLUGIN_ID
			+ ".TESTNAME"; //$NON-NLS-1$

	public static final String ATTR_KEEPRUNNING = GUnitPlugin.PLUGIN_ID
			+ ".KEEPRUNNING_ATTR"; //$NON-NLS-1$

	/**
	 * The launch container, or "" iff running a single test type.
	 */
	public static final String ATTR_TEST_CONTAINER = GUnitPlugin.PLUGIN_ID
			+ ".CONTAINER"; //$NON-NLS-1$

	public static final String ATTR_FAILURES_NAMES = GUnitPlugin.PLUGIN_ID
			+ ".FAILURENAMES"; //$NON-NLS-1$

	public static final String ATTR_TEST_RUNNER_KIND = GUnitPlugin.PLUGIN_ID
			+ ".TEST_KIND"; //$NON-NLS-1$

	public static ITestKind getTestRunnerKind(
			ILaunchConfiguration launchConfiguration) {
		try {
			String loaderId = launchConfiguration.getAttribute(
					GUnitLaunchConfigurationConstants.ATTR_TEST_RUNNER_KIND,
					(String) null);
			if (loaderId != null) {
				return TestKindRegistry.getDefault().getKind(loaderId);
			}
		} catch (CoreException e) {
		}
		return ITestKind.NULL;
	}

	public static IErlProject getErlProject(ILaunchConfiguration configuration) {
		// try {
		// String projectName = configuration.getAttribute(
		// IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME,
		// (String) null);
		// if (projectName != null && projectName.length() > 0) {
		// return JavaCore.create(ResourcesPlugin.getWorkspace().getRoot()
		// .getProject(projectName));
		// }
		// } catch (CoreException e) {
		// }
		return null;
	}

}
