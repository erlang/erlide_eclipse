/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.erlide.gunit.internal.model;

import org.eclipse.core.runtime.Assert;
import org.erlide.gunit.model.ITestCaseElement;

public class TestCaseElement extends TestElement implements ITestCaseElement {

	private boolean fIgnored;

	public TestCaseElement(final TestSuiteElement parent, final String id, final String testName) {
		super(parent, id, testName);
		Assert.isNotNull(parent);
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @see org.erlide.gunit.model.ITestCaseElement#getTestMethodName()
	 * @see org.erlide.gunit.internal.runner.MessageIds#TEST_IDENTIFIER_MESSAGE_FORMAT
	 * @see org.erlide.gunit.internal.runner.MessageIds#IGNORED_TEST_PREFIX
	 */
	public String getTestMethodName() {
		final String testName = getTestName();
		int index = testName.indexOf('(');
		if (index > 0) {
			return testName.substring(0, index);
		}
		index = testName.indexOf('@');
		if (index > 0) {
			return testName.substring(0, index);
		}
		return testName;
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @see org.erlide.gunit.model.ITestCaseElement#getTestClassName()
	 */
	public String getTestClassName() {
		return getClassName();
	}

	public void setIgnored(final boolean ignored) {
		this.fIgnored = ignored;
	}

	public boolean isIgnored() {
		return this.fIgnored;
	}

	@Override
	public String toString() {
		return "TestCase: " + getTestClassName() + "." + getTestMethodName() + " : " + super.toString(); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
	}
}
