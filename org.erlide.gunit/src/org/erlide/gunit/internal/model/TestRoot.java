/*******************************************************************************
 * Copyright (c) 2005, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.gunit.internal.model;

import org.erlide.gunit.model.ITestRunSession;

public class TestRoot extends TestSuiteElement {

	private final ITestRunSession fSession;

	public TestRoot(ITestRunSession session) {
		super(null, "-1", session.getTestRunName(), 1); //$NON-NLS-1$
		this.fSession = session;
	}

	@Override
	public TestRoot getRoot() {
		return this;
	}

	@Override
	public ITestRunSession getTestRunSession() {
		return this.fSession;
	}
}
