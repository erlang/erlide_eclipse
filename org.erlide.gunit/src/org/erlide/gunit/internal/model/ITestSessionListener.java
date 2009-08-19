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

import org.erlide.gunit.model.TestElement;
import org.erlide.gunit.model.TestElement.Status;

/**
 * A listener interface for observing the execution of a test session (initial
 * run and reruns).
 */
public interface ITestSessionListener {
	/**
	 * A test run has started.
	 */
	public void sessionStarted();

	/**
	 * A test run has ended.
	 * 
	 * @param elapsedTime
	 *            the total elapsed time of the test run
	 */
	public void sessionEnded(long elapsedTime);

	/**
	 * A test run has been stopped prematurely.
	 * 
	 * @param elapsedTime
	 *            the time elapsed before the test run was stopped
	 */
	public void sessionStopped(long elapsedTime);

	/**
	 * The VM instance performing the tests has terminated.
	 */
	public void sessionTerminated();

	/**
	 * A test has been added to the plan.
	 * 
	 * @param testElement
	 *            the test
	 */
	public void testAdded(TestElement testElement);

	/**
	 * All test have been added and running begins
	 */
	public void runningBegins();

	/**
	 * An individual test has started.
	 * 
	 * @param testCaseElement
	 *            the test
	 */
	public void testStarted(TestCaseElement testCaseElement);

	/**
	 * An individual test has ended.
	 * 
	 * @param testCaseElement
	 *            the test
	 */
	public void testEnded(TestCaseElement testCaseElement);

	/**
	 * An individual test has failed with a stack trace.
	 * 
	 * @param testElement
	 *            the test
	 * @param status
	 *            the outcome of the test; one of
	 *            {@link TestElement.Status#ERROR} or
	 *            {@link TestElement.Status#FAILURE}
	 * @param trace
	 *            the stack trace
	 * @param expected
	 *            expected value
	 * @param actual
	 *            actual value
	 */
	public void testFailed(TestElement testElement, Status status,
			String trace, String expected, String actual);

	/**
	 * An individual test has been rerun.
	 * 
	 * @param testCaseElement
	 *            the test
	 * @param status
	 *            the outcome of the test that was rerun; one of
	 *            {@link TestElement.Status#OK},
	 *            {@link TestElement.Status#ERROR}, or
	 *            {@link TestElement.Status#FAILURE}
	 * @param trace
	 *            the stack trace in the case of abnormal termination, or the
	 *            empty string if none
	 * @param expectedResult
	 *            expected value
	 * @param actualResult
	 *            actual value
	 */
	public void testReran(TestCaseElement testCaseElement, Status status,
			String trace, String expectedResult, String actualResult);

	/**
	 * @return <code>true</code> if the test run session can be swapped to disk
	 *         although this listener is still installed
	 */
	public boolean acceptsSwapToDisk();

}
