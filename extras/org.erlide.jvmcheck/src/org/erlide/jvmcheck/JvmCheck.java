/*******************************************************************************
 * Copyright (c) 2015, 2015 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Bruno Medeiros - initial API and implementation
 *******************************************************************************/
package org.erlide.jvmcheck;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

public class JvmCheck implements IStartup, JvmCheckConstants_Actual {

	public static String getJavaVersionProperty() {
		return System.getProperty("java.version");
	}

	public static int getJavaVersion() {
		final String versionProperty = JvmCheck.getJavaVersionProperty();
		final String[] versionSegments = versionProperty.split("\\.");

		// v<9: 1.8...
		// v>9: 9.x...
		String javaVersionStr = versionSegments[0];
		try {
			final int v = Integer.parseInt(javaVersionStr);
			if (v == 1) {
				javaVersionStr = versionSegments[1];
				return Integer.parseInt(javaVersionStr);
			}
			return v;
		} catch (final NumberFormatException e) {
			return -1;
		}
	}

	@Override
	public void earlyStartup() {
		final int javaVersion = JvmCheck.getJavaVersion();

		if (javaVersion >= JvmCheckConstants_Actual.REQUIRED_JAVA_VERSION) {
			return;
		}

		// Show error message to the user, because the platform just silently fails.
		// See: https://bugs.eclipse.org/bugs/show_bug.cgi?id=417336

		Display.getDefault().asyncExec(new Runnable() {
			@Override
			public void run() {
				final Shell activeShell = JvmCheck.getActiveWorkbenchShell();

				final String message = "Could not start " + JvmCheckConstants_Actual.FEATURE_NAME
						+ " because Java version is: " + javaVersion + "\nVersion "
						+ JvmCheckConstants_Actual.REQUIRED_JAVA_VERSION + " or later is required";

				System.err.println(message);

				if (activeShell == null) {
					return;
				}
				MessageDialog.openError(activeShell, "Error", message);
			}
		});

	}

	/** Gets the active workbench window. */
	public static IWorkbenchWindow getActiveWorkbenchWindow() {
		return PlatformUI.getWorkbench().getActiveWorkbenchWindow();
	}

	/** Gets the active workbench shell. */
	public static Shell getActiveWorkbenchShell() {
		final IWorkbenchWindow window = JvmCheck.getActiveWorkbenchWindow();
		if (window != null) {
			return window.getShell();
		}
		return null;
	}

}