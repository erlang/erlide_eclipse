/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.erlide.gunit;

import org.erlide.gunit.internal.ui.GUnitPlugin;

/**
 * Class for accessing JUnit support; all functionality is provided by static
 * methods.
 * <p>
 * This class is not intended to be subclassed or instantiated by clients.
 * </p>
 * 
 * @since 2.1
 */
public class GUnitCore {

	/**
	 * Adds a listener for test runs.
	 * 
	 * @param listener
	 *            listener to be added
	 * @deprecated As of 3.3, replaced by
	 *             {@link #addTestRunListener(TestRunListener)}
	 */

	/**
	 * Adds a listener for test runs.
	 * 
	 * @param listener
	 *            the listener to be added
	 * @since 3.3
	 */
	public static void addTestRunListener(final TestRunListener listener) {
		GUnitPlugin.getDefault().getNewTestRunListeners().add(listener);
	}

	/**
	 * Removes a listener for test runs.
	 * 
	 * @param listener
	 *            the listener to be removed
	 * @since 3.3
	 */
	public static void removeTestRunListener(final TestRunListener listener) {
		GUnitPlugin.getDefault().getNewTestRunListeners().remove(listener);
	}
}
