/*******************************************************************************
 * Copyright (c) 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     David Saff (saff@mit.edu) - initial API and implementation
 *             (bug 102632: [JUnit] Support for JUnit 4.)
 *******************************************************************************/

package org.erlide.gunit.servers.launcher;

import org.erlide.gunit.internal.launcher.ITestFinder;

public interface ITestKind {
	static class NullTestKind extends TestKind {
		private NullTestKind() {
			super(null);
		}

		@Override
		public boolean isNull() {
			return true;
		}

		@Override
		protected String getAttribute(String arg0) {
			return null;
		}

		@Override
		public ITestFinder getFinder() {
			return ITestFinder.NULL;
		}

	}

	public static final TestKind NULL = new NullTestKind();

	public static final String ID = "id"; //$NON-NLS-1$

	public static final String DISPLAY_NAME = "displayName"; //$NON-NLS-1$

	public static final String FINDER_CLASS_NAME = "finderClass"; //$NON-NLS-1$

	public static final String LOADER_PLUGIN_ID = "loaderPluginId"; //$NON-NLS-1$

	public static final String LOADER_CLASS_NAME = "loaderClass"; //$NON-NLS-1$

	public static final String PRECEDES = "precedesTestKind"; //$NON-NLS-1$

	public static final String RUNTIME_CLASSPATH_ENTRY = "runtimeClasspathEntry"; //$NON-NLS-1$

	public static final String CLASSPATH_PLUGIN_ID = "pluginId"; //$NON-NLS-1$

	public static final String CLASSPATH_PATH_TO_JAR = "pathToJar"; //$NON-NLS-1$

	public abstract ITestFinder getFinder();

	public abstract String getId();

	public abstract String getDisplayName();

	public abstract String getFinderClassName();

	public abstract String getLoaderPluginId();

	public abstract String getLoaderClassName();

	public abstract String getPrecededKindId();

	public abstract boolean isNull();

}