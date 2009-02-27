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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.erlide.gunit.internal.launcher.ITestFinder;
import org.erlide.gunit.internal.ui.GUnitPlugin;

public class TestKind implements ITestKind {

	private final IConfigurationElement fElement;

	private ITestFinder fFinder;

	public TestKind(IConfigurationElement element) {
		this.fElement = element;
		this.fFinder = null;
	}

	/*
	 * @see org.erlide.gunit.internal.launcher.ITestKind#createFinder()
	 */
	public ITestFinder getFinder() {
		if (this.fFinder == null) {
			try {
				this.fFinder = (ITestFinder) this.fElement
						.createExecutableExtension(FINDER_CLASS_NAME);
			} catch (CoreException e1) {
				GUnitPlugin.log(e1);
				this.fFinder = ITestFinder.NULL;
			}
		}
		return this.fFinder;
	}

	/*
	 * @see org.erlide.gunit.internal.launcher.ITestKind#getDisplayName()
	 */
	public String getDisplayName() {
		return getAttribute(DISPLAY_NAME);
	}

	/*
	 * @see org.erlide.gunit.internal.launcher.ITestKind#getFinderClassName()
	 */
	public String getFinderClassName() {
		return getAttribute(FINDER_CLASS_NAME);
	}

	/*
	 * @see org.erlide.gunit.internal.launcher.ITestKind#getId()
	 */
	public String getId() {
		return getAttribute(ID);
	}

	/*
	 * @see org.erlide.gunit.internal.launcher.ITestKind#getLoaderClassName()
	 */
	public String getLoaderClassName() {
		return getAttribute(LOADER_CLASS_NAME);
	}

	public String getLoaderPluginId() {
		return getAttribute(LOADER_PLUGIN_ID);
	}

	/*
	 * @see org.erlide.gunit.internal.launcher.ITestKind#getPrecededKindId()
	 */
	public String getPrecededKindId() {
		String attribute = getAttribute(PRECEDES);
		return attribute == null ? "" : attribute; //$NON-NLS-1$
	}

	/*
	 * @see org.erlide.gunit.internal.launcher.ITestKind#isNull()
	 */
	public boolean isNull() {
		return false;
	}

	protected String getAttribute(String attributeName) {
		return this.fElement.getAttribute(attributeName);
	}

	boolean precedes(ITestKind otherKind) {
		final String precededKindId = getPrecededKindId();
		String[] ids = precededKindId.split(","); //$NON-NLS-1$
		for (int i = 0; i < ids.length; i++) {
			if (ids[i].equals(otherKind.getId())) {
				return true;
			}
		}
		return false;
	}

	/*
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return getDisplayName() + " (id: " + getId() + ")"; //$NON-NLS-1$ //$NON-NLS-2$
	}
}
