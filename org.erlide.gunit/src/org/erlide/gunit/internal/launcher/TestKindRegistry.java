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
	private ArrayList<ITestKind> fTestKinds;

	private TestKindRegistry(IExtensionPoint point) {
		fPoint = point;
	}

	public ArrayList<ITestKind> getAllKinds() {
		loadKinds();
		return fTestKinds;
	}

	private void loadKinds() {
		if (fTestKinds != null) {
			return;
		}

		ArrayList items = new ArrayList();
		for (Iterator iter = getConfigurationElements().iterator(); iter
				.hasNext();) {
			IConfigurationElement element = (IConfigurationElement) iter.next();
			items.add(new TestKind(element));
		}

		Collections.sort(items, new Comparator() {
			public int compare(Object arg0, Object arg1) {
				TestKind kind0 = (TestKind) arg0;
				TestKind kind1 = (TestKind) arg1;

				if (kind0.precedes(kind1)) {
					return -1;
				}
				if (kind1.precedes(kind0)) {
					return 1;
				}
				return 0;
			}
		});
		fTestKinds = items;
	}

	public ArrayList/* <String> */getDisplayNames() {
		ArrayList result = new ArrayList();
		ArrayList testTypes = getAllKinds();
		for (Iterator iter = testTypes.iterator(); iter.hasNext();) {
			ITestKind type = (ITestKind) iter.next();
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
		IExtension[] extensions = fPoint.getExtensions();
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
		ArrayList<ITestKind> allKinds = getAllKinds();
		String returnThis = ""; //$NON-NLS-1$
		for (Iterator<ITestKind> iter = allKinds.iterator(); iter.hasNext();) {
			ITestKind kind = iter.next();
			returnThis += "(" + kind.getId() + ")"; //$NON-NLS-1$ //$NON-NLS-2$
		}
		return returnThis;
	}
}
