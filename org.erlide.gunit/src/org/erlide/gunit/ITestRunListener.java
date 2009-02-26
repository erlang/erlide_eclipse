/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.erlide.gunit;

import org.erlide.gunit.internal.model.ITestRunListener2;

/**
 * A listener interface for observing the execution of a test run.
 * <p>
 * Clients contributing to the <code>org.erlide.gunit.testRunListener</code>
 * extension point implement this interface.
 * </p>
 * 
 * @deprecated As of 3.3, replaced by {@link TestRunListener}
 * 
 * @since 2.1
 */
@Deprecated
public interface ITestRunListener {
	/**
	 * Status constant indicating that a test passed (constant value 0).
	 * 
	 * @see #testFailed(int, String, String, String)
	 */
	public static final int STATUS_OK = ITestRunListener2.STATUS_OK;
	/**
	 * Status constant indicating that a test had an error an unanticipated
	 * exception (constant value 1).
	 * 
	 * @see #testFailed(int, String, String, String)
	 */
	public static final int STATUS_ERROR = ITestRunListener2.STATUS_ERROR;
	/**
	 * Status constant indicating that a test failed an assertion (constant
	 * value 2).
	 * 
	 * @see #testFailed(int, String, String, String)
	 */
	public static final int STATUS_FAILURE = ITestRunListener2.STATUS_FAILURE;

	/**
	 * A test run has started.
	 * 
	 * @param testCount
	 *            the number of individual tests that will be run
	 */
	public void testRunStarted(int testCount);

	/**
	 * A test run has ended.
	 * 
	 * @param elapsedTime
	 *            the total elapsed time of the test run
	 */
	public void testRunEnded(long elapsedTime);

	/**
	 * A test run has been stopped prematurely.
	 * 
	 * @param elapsedTime
	 *            the time elapsed before the test run was stopped
	 */
	public void testRunStopped(long elapsedTime);

	/**
	 * An individual test has started.
	 * 
	 * @param testId
	 *            a unique Id identifying the test
	 * @param testName
	 *            the name of the test that started
	 */
	public void testStarted(String testId, String testName);

	/**
	 * An individual test has ended.
	 * 
	 * @param testId
	 *            a unique Id identifying the test
	 * @param testName
	 *            the name of the test that ended
	 */
	public void testEnded(String testId, String testName);

	/**
	 * An individual test has failed with a stack trace.
	 * 
	 * @param status
	 *            the outcome of the test; one of {@link #STATUS_ERROR
	 *            STATUS_ERROR} or {@link #STATUS_FAILURE STATUS_FAILURE}
	 * @param testId
	 *            a unique Id identifying the test
	 * @param testName
	 *            the name of the test that failed
	 * @param trace
	 *            the stack trace
	 */
	public void testFailed(int status, String testId, String testName,
			String trace);

	/**
	 * The VM instance performing the tests has terminated.
	 */
	public void testRunTerminated();

	/**
	 * An individual test has been rerun.
	 * 
	 * @param testId
	 *            a unique Id identifying the test
	 * @param testClass
	 *            the name of the test class that was rerun
	 * @param testName
	 *            the name of the test that was rerun
	 * @param status
	 *            the outcome of the test that was rerun; one of
	 *            {@link #STATUS_OK STATUS_OK}, {@link #STATUS_ERROR
	 *            STATUS_ERROR}, or {@link #STATUS_FAILURE STATUS_FAILURE}
	 * @param trace
	 *            the stack trace in the case of abnormal termination, or the
	 *            empty string if none
	 */
	public void testReran(String testId, String testClass, String testName,
			int status, String trace);
}
