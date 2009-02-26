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
package org.erlide.gunit.model;

/**
 * Common protocol for test elements. This set consists of
 * {@link ITestCaseElement} , {@link ITestSuiteElement} and
 * {@link ITestRunSession}
 * 
 * <p>
 * This interface is not intended to be implemented by clients.
 * </p>
 * 
 * 
 * @since 3.3
 */
public interface ITestElement {

	/**
	 * Running states of a test.
	 */
	public static final class ProgressState {
		/** state that describes that the test element has not started */
		public static final ProgressState NOT_STARTED = new ProgressState(
				"Not Started"); //$NON-NLS-1$
		/** state that describes that the test element has is running */
		public static final ProgressState RUNNING = new ProgressState("Running"); //$NON-NLS-1$
		/**
		 * state that describes that the test element has been stopped before
		 * being completed
		 */
		public static final ProgressState STOPPED = new ProgressState("Stopped"); //$NON-NLS-1$
		/** state that describes that the test element has completed */
		public static final ProgressState COMPLETED = new ProgressState(
				"Completed"); //$NON-NLS-1$

		private String fName;

		private ProgressState(String name) {
			fName = name;
		}

		@Override
		public String toString() {
			return fName;
		}
	}

	/**
	 * Result states of a test.
	 */
	public static final class Result {
		/** state that describes that the test result is undefined */
		public static final Result UNDEFINED = new Result("Undefined"); //$NON-NLS-1$
		/** state that describes that the test result is 'OK' */
		public static final Result OK = new Result("OK"); //$NON-NLS-1$
		/** state that describes that the test result is 'Error' */
		public static final Result ERROR = new Result("Error"); //$NON-NLS-1$
		/** state that describes that the test result is 'Failure' */
		public static final Result FAILURE = new Result("Failure"); //$NON-NLS-1$
		/** state that describes that the test result is 'Ignored' */
		public static final Result IGNORED = new Result("Ignored"); //$NON-NLS-1$

		private String fName;

		private Result(String name) {
			fName = name;
		}

		@Override
		public String toString() {
			return fName;
		}
	}

	/**
	 * A failure trace of a test.
	 * 
	 * This class is not intended to be instantiated or extended by clients.
	 */
	public static final class FailureTrace {
		private final String fActual;
		private final String fExpected;
		private final String fTrace;

		public FailureTrace(String trace, String expected, String actual) {
			fActual = actual;
			fExpected = expected;
			fTrace = trace;
		}

		/**
		 * Returns the failure stack trace.
		 * 
		 * @return the failure stack trace
		 */
		public String getTrace() {
			return fTrace;
		}

		/**
		 * Returns the expected result or <code>null</code> if the trace is not
		 * a comparison failure.
		 * 
		 * @return the expected result or <code>null</code> if the trace is not
		 *         a comparison failure.
		 */
		public String getExpected() {
			return fExpected;
		}

		/**
		 * Returns the actual result or <code>null</code> if the trace is not a
		 * comparison failure.
		 * 
		 * @return the actual result or <code>null</code> if the trace is not a
		 *         comparison failure.
		 */
		public String getActual() {
			return fActual;
		}
	}

	/**
	 * Returns the progress state of this test element.
	 * <dl>
	 * <li>{@link ITestElement.ProgressState#NOT_STARTED}: the test has not yet
	 * started</li>
	 * <li>{@link ITestElement.ProgressState#RUNNING}: the test is currently
	 * running</li>
	 * <li>{@link ITestElement.ProgressState#STOPPED}: the test has stopped
	 * before being completed</li>
	 * <li>{@link ITestElement.ProgressState#COMPLETED}: the test (and all its
	 * children) has completed</li>
	 * </dl>
	 * 
	 * @return returns one of {@link ITestElement.ProgressState#NOT_STARTED},
	 *         {@link ITestElement.ProgressState#RUNNING},
	 *         {@link ITestElement.ProgressState#STOPPED} or
	 *         {@link ITestElement.ProgressState#COMPLETED}.
	 */
	public ProgressState getProgressState();

	/**
	 * Returns the result of the test element.
	 * <dl>
	 * <li>{@link ITestElement.Result#UNDEFINED}: the result is not yet
	 * evaluated</li>
	 * <li>{@link ITestElement.Result#OK}: the test has succeeded</li>
	 * <li>{@link ITestElement.Result#ERROR}: the test has returned an error</li>
	 * <li>{@link ITestElement.Result#FAILURE}: the test has returned an failure
	 * </li>
	 * <li>{@link ITestElement.Result#IGNORED}: the test has been ignored
	 * (skipped)</li>
	 * </dl>
	 * 
	 * @param includeChildren
	 *            if <code>true</code>, the returned result is the combined
	 *            result of the test and its children (if it has any). If
	 *            <code>false</code>, only the test's result is returned.
	 * 
	 * @return returns one of {@link ITestElement.Result#UNDEFINED},
	 *         {@link ITestElement.Result#OK}, {@link ITestElement.Result#ERROR}
	 *         , {@link ITestElement.Result#FAILURE} or
	 *         {@link ITestElement.Result#IGNORED}. Clients should also prepare
	 *         for other, new values.
	 */
	public Result getTestResult(boolean includeChildren);

	/**
	 * Returns the failure trace of this test element or <code>null</code> if
	 * the test has not resulted in an error or failure.
	 * 
	 * @return the failure trace of this test or <code>null</code>.
	 */
	public FailureTrace getFailureTrace();

	/**
	 * Returns the parent test element container or <code>null</code> if the
	 * test element is the test run session.
	 * 
	 * @return the parent test suite
	 */
	public ITestElementContainer getParentContainer();

	/**
	 * Returns the test run session.
	 * 
	 * @return the parent test run session.
	 */
	public ITestRunSession getTestRunSession();

}
