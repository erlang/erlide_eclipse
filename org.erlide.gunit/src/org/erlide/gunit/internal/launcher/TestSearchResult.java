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

package org.erlide.gunit.internal.launcher;

import org.erlide.core.erlang.IErlModule;

public class TestSearchResult {

	private final IErlModule[] fTypes;

	private final ITestKind fTestKind;

	public TestSearchResult(IErlModule[] types, ITestKind testKind) {
		this.fTypes = types;
		this.fTestKind = testKind;
	}

	public IErlModule[] getTypes() {
		return this.fTypes;
	}

	public ITestKind getTestKind() {
		return this.fTestKind;
	}

	boolean isEmpty() {
		return getTypes().length <= 0;
	}
}
