/*******************************************************************************
 * Copyright (c) 2006, 2007 IBM Corporation and others.
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

package org.erlide.gunit.internal.launcher;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;
import org.erlide.core.erlang.IErlElement;
import org.erlide.gunit.internal.ui.GUnitPlugin;

public class TestKindRegistry {

	public static TestKindRegistry getDefault() {
		if (fgRegistry != null) {
			return fgRegistry;
		}

		fgRegistry = new TestKindRegistry(Platform.getExtensionRegistry()
				.getExtensionPoint(GUnitPlugin.ID_EXTENSION_POINT_TEST_KINDS));
		return fgRegistry;
	}

	private static TestKindRegistry fgRegistry;

	private final IExtensionPoint fPoint;

	private ArrayList<TestKind> fTestKinds;

	private TestKindRegistry(IExtensionPoint point) {
		this.fPoint = point;
	}

	public List<TestKind> getAllKinds() {
		loadKinds();
		return this.fTestKinds;
	}

	private void loadKinds() {
		if (this.fTestKinds != null) {
			return;
		}

		ArrayList<TestKind> items = new ArrayList<TestKind>();
		for (Iterator iter = getConfigurationElements().iterator(); iter
				.hasNext();) {
			IConfigurationElement element = (IConfigurationElement) iter.next();
			items.add(new TestKind(element));
		}

		Collections.sort(items, new Comparator<TestKind>() {
			public int compare(TestKind kind0, TestKind kind1) {

				if (kind0.precedes(kind1)) {
					return -1;
				}
				if (kind1.precedes(kind0)) {
					return 1;
				}
				return 0;
			}
		});
		this.fTestKinds = items;
	}

	public List<String> getDisplayNames() {
		ArrayList<String> result = new ArrayList<String>();
		List<TestKind> testTypes = getAllKinds();
		for (Iterator<TestKind> iter = testTypes.iterator(); iter.hasNext();) {
			ITestKind type = iter.next();
			result.add(type.getDisplayName());
		}
		return result;
	}

	/**
	 * @param testKindId
	 *            an id, can be <code>null</code>
	 * @return a TestKind, ITestKind.NULL if not available
	 */
	public ITestKind getKind(String testKindId) {
		if (testKindId != null) {
			for (Iterator iter = getAllKinds().iterator(); iter.hasNext();) {
				TestKind kind = (TestKind) iter.next();
				if (testKindId.equals(kind.getId())) {
					return kind;
				}
			}
		}
		return ITestKind.NULL;
	}

	public static String getContainerTestKindId(IErlElement element) {
		// FIXME
		return "";
	}

	public static ITestKind getContainerTestKind(IErlElement element) {
		return getDefault().getKind(getContainerTestKindId(element));
	}

	private ArrayList<IConfigurationElement> getConfigurationElements() {
		ArrayList<IConfigurationElement> items = new ArrayList<IConfigurationElement>();
		IExtension[] extensions = this.fPoint.getExtensions();
		for (int i = 0; i < extensions.length; i++) {
			IExtension extension = extensions[i];
			IConfigurationElement[] elements = extension
					.getConfigurationElements();
			for (int j = 0; j < elements.length; j++) {
				IConfigurationElement element = elements[j];
				items.add(element);
			}
		}
		return items;
	}

	public String getAllKindIds() {
		List<TestKind> allKinds = getAllKinds();
		String returnThis = ""; //$NON-NLS-1$
		for (Iterator<TestKind> iter = allKinds.iterator(); iter.hasNext();) {
			ITestKind kind = iter.next();
			returnThis += "(" + kind.getId() + ")"; //$NON-NLS-1$ //$NON-NLS-2$
		}
		return returnThis;
	}
}
