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

	private TestKindRegistry(final IExtensionPoint point) {
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

		final ArrayList<TestKind> items = new ArrayList<TestKind>();
		for (final Iterator<IConfigurationElement> iter = getConfigurationElements()
				.iterator(); iter.hasNext();) {
			final IConfigurationElement element = iter.next();
			items.add(new TestKind(element));
		}

		Collections.sort(items, new Comparator<TestKind>() {
			public int compare(final TestKind kind0, final TestKind kind1) {

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
		final ArrayList<String> result = new ArrayList<String>();
		final List<TestKind> testTypes = getAllKinds();
		for (final Iterator<TestKind> iter = testTypes.iterator(); iter.hasNext();) {
			final ITestKind type = iter.next();
			result.add(type.getDisplayName());
		}
		return result;
	}

	/**
	 * @param testKindId
	 *            an id, can be <code>null</code>
	 * @return a TestKind, ITestKind.NULL if not available
	 */
	public ITestKind getKind(final String testKindId) {
		if (testKindId != null) {
			for (final Iterator<TestKind> iter = getAllKinds().iterator(); iter
			.hasNext();) {
				final TestKind kind = iter.next();
				if (testKindId.equals(kind.getId())) {
					return kind;
				}
			}
		}
		return ITestKind.NULL;
	}

	public static String getContainerTestKindId(final IErlElement element) {
		// FIXME
		return "";
	}

	public static ITestKind getContainerTestKind(final IErlElement element) {
		return getDefault().getKind(getContainerTestKindId(element));
	}

	private ArrayList<IConfigurationElement> getConfigurationElements() {
		final ArrayList<IConfigurationElement> items = new ArrayList<IConfigurationElement>();
		final IExtension[] extensions = this.fPoint.getExtensions();
		for (int i = 0; i < extensions.length; i++) {
			final IExtension extension = extensions[i];
			final IConfigurationElement[] elements = extension
			.getConfigurationElements();
			for (int j = 0; j < elements.length; j++) {
				final IConfigurationElement element = elements[j];
				items.add(element);
			}
		}
		return items;
	}

	public String getAllKindIds() {
		final List<TestKind> allKinds = getAllKinds();
		String returnThis = ""; //$NON-NLS-1$
		for (final Iterator<TestKind> iter = allKinds.iterator(); iter.hasNext();) {
			final ITestKind kind = iter.next();
			returnThis += "(" + kind.getId() + ")"; //$NON-NLS-1$ //$NON-NLS-2$
		}
		return returnThis;
	}
}
