/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     David Saff (saff@mit.edu) - bug 102632: [JUnit] Support for JUnit 4.
 *******************************************************************************/
package org.erlide.gunit.internal.ui;

import java.util.Arrays;
import java.util.List;

/**
 * Defines constants which are used to refer to values in the plugin's
 * preference store.
 */
public class GUnitPreferencesConstants {
	/**
	 * Boolean preference controlling whether the failure stack should be
	 * filtered.
	 */
	public static final String DO_FILTER_STACK = GUnitPlugin.PLUGIN_ID
			+ ".do_filter_stack"; //$NON-NLS-1$

	/**
	 * Boolean preference controlling whether the JUnit view should be shown on
	 * errors only.
	 */
	public static final String SHOW_ON_ERROR_ONLY = GUnitPlugin.PLUGIN_ID
			+ ".show_on_error"; //$NON-NLS-1$

	/**
	 * Boolean preference controlling whether the JUnit view should be shown on
	 * errors only.
	 */
	public static final String ENABLE_ASSERTIONS = GUnitPlugin.PLUGIN_ID
			+ ".enable_assertions"; //$NON-NLS-1$

	/**
	 * List of active stack filters. A String containing a comma separated list
	 * of fully qualified type names/patterns.
	 */
	public static final String PREF_ACTIVE_FILTERS_LIST = GUnitPlugin.PLUGIN_ID
			+ ".active_filters"; //$NON-NLS-1$

	/**
	 * List of inactive stack filters. A String containing a comma separated
	 * list of fully qualified type names/patterns.
	 */
	public static final String PREF_INACTIVE_FILTERS_LIST = GUnitPlugin.PLUGIN_ID
			+ ".inactive_filters"; //$NON-NLS-1$	

	/**
	 * Maximum number of remembered test runs.
	 */
	public static final String MAX_TEST_RUNS = GUnitPlugin.PLUGIN_ID
			+ ".max_test_runs"; //$NON-NLS-1$

	/**
	 * Javadoc location for JUnit 3
	 */
	public static final String JUNIT3_JAVADOC = GUnitPlugin.PLUGIN_ID
			+ ".junit3.javadoclocation"; //$NON-NLS-1$

	/**
	 * Javadoc location for JUnit 4
	 */
	public static final String JUNIT4_JAVADOC = GUnitPlugin.PLUGIN_ID
			+ ".junit4.javadoclocation"; //$NON-NLS-1$

	private static final String[] fgDefaultFilterPatterns = new String[] {
			"org.erlide.gunit.internal.runner.*", //$NON-NLS-1$
			"org.eclipse.jdt.internal.junit4.runner.*", //$NON-NLS-1$
			"org.erlide.gunit.internal.ui.*", //$NON-NLS-1$
			"junit.framework.TestCase", //$NON-NLS-1$
			"junit.framework.TestResult", //$NON-NLS-1$
			"junit.framework.TestResult$1", //$NON-NLS-1$
			"junit.framework.TestSuite", //$NON-NLS-1$
			"junit.framework.Assert", //$NON-NLS-1$
			"org.junit.*", //$NON-NLS-1$ //TODO: filter all these?
			"java.lang.reflect.Method.invoke", //$NON-NLS-1$
			"sun.reflect.*", //$NON-NLS-1$
	};

	private GUnitPreferencesConstants() {
		// no instance
	}

	/**
	 * Returns the default list of active stack filters.
	 * 
	 * @return list
	 */
	public static List createDefaultStackFiltersList() {
		return Arrays.asList(fgDefaultFilterPatterns);
	}

	/**
	 * Serializes the array of strings into one comma separated string.
	 * 
	 * @param list
	 *            array of strings
	 * @return a single string composed of the given list
	 */
	public static String serializeList(String[] list) {
		if (list == null) {
			return ""; //$NON-NLS-1$
		}

		StringBuffer buffer = new StringBuffer();
		for (int i = 0; i < list.length; i++) {
			if (i > 0) {
				buffer.append(',');
			}

			buffer.append(list[i]);
		}
		return buffer.toString();
	}
}
