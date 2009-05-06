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
package org.erlide.gunit;

import org.erlide.gunit.model.ITestCaseElement;
import org.erlide.gunit.model.ITestRunSession;

/**
 * A test run listener that can be registered at
 * {@link GUnitCore#addTestRunListener(TestRunListener)}.
 * <p>
 * A test run starts with the call to {@link #sessionStarted(ITestRunSession)}
 * followed by calls to {@link #testCaseStarted(ITestCaseElement)} and
 * {@link #testCaseFinished(ITestCaseElement)} for all test cases contained in
 * the tree.
 * </p>
 * <p>
 * A test run session is ended with the call to
 * {@link #sessionFinished(ITestRunSession)}. After that call, no references
 * must be kept to the session or any of the test cases or suites.
 * </p>
 * 
 * @since 3.3
 */
public abstract class TestRunListener {

	/**
	 * A test run session has started. The test tree can be accessed through the
	 * session element.
	 * <p>
	 * Important: The implementor of this method must not keep a reference to
	 * the session element longer after
	 * {@link #sessionFinished(ITestRunSession)} has finished.
	 * </p>
	 * 
	 * @param session
	 *            the session that has just started.
	 */
	public void sessionStarted(final ITestRunSession session) {
	}

	/**
	 * A test run session has finished. The test tree can be accessed through
	 * the session element.
	 * 
	 * <p>
	 * Important: The implementor of this method must not keep the session
	 * element when the method is finished.
	 * </p>
	 * 
	 * @param session
	 *            the test
	 */
	public void sessionFinished(final ITestRunSession session) {
	}

	/**
	 * A test case has started. The result can be accessed from the element.
	 * <p>
	 * Important: The implementor of this method must not keep a reference to
	 * the test case element after {@link #sessionFinished(ITestRunSession)} has
	 * finished.
	 * </p>
	 * 
	 * @param testCaseElement
	 *            the test that has started to run
	 */
	public void testCaseStarted(final ITestCaseElement testCaseElement) {
	}

	/**
	 * A test case has ended. The result can be accessed from the element.
	 * <p>
	 * Important: The implementor of this method must not keep a reference to
	 * the test case element after {@link #sessionFinished(ITestRunSession)} has
	 * finished.
	 * </p>
	 * 
	 * @param testCaseElement
	 *            the test that has finished running
	 */
	public void testCaseFinished(final ITestCaseElement testCaseElement) {
	}
}
