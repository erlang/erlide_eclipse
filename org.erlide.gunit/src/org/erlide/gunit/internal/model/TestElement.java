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
import org.erlide.gunit.model.ITestElement;
import org.erlide.gunit.model.ITestElementContainer;
import org.erlide.gunit.model.ITestRunSession;

public abstract class TestElement implements ITestElement {
	public final static class Status {
		public static final Status RUNNING_ERROR = new Status(
				"RUNNING_ERROR", 5); //$NON-NLS-1$

		public static final Status RUNNING_FAILURE = new Status(
				"RUNNING_FAILURE", 6); //$NON-NLS-1$

		public static final Status RUNNING = new Status("RUNNING", 3); //$NON-NLS-1$

		public static final Status ERROR = new Status(
				"ERROR", /* 1 */ITestRunListener2.STATUS_ERROR); //$NON-NLS-1$

		public static final Status FAILURE = new Status(
				"FAILURE", /* 2 */ITestRunListener2.STATUS_FAILURE); //$NON-NLS-1$

		public static final Status OK = new Status(
				"OK", /* 0 */ITestRunListener2.STATUS_OK); //$NON-NLS-1$

		public static final Status NOT_RUN = new Status("NOT_RUN", 4); //$NON-NLS-1$

		private static final Status[] OLD_CODE = { OK, ERROR, FAILURE };

		private final String fName;

		private final int fOldCode;

		private Status(String name, int oldCode) {
			this.fName = name;
			this.fOldCode = oldCode;
		}

		public int getOldCode() {
			return this.fOldCode;
		}

		@Override
		public String toString() {
			return this.fName;
		}

		/* error state predicates */

		public boolean isOK() {
			return this == OK || this == RUNNING || this == NOT_RUN;
		}

		public boolean isFailure() {
			return this == FAILURE || this == RUNNING_FAILURE;
		}

		public boolean isError() {
			return this == ERROR || this == RUNNING_ERROR;
		}

		public boolean isErrorOrFailure() {
			return isError() || isFailure();
		}

		/* progress state predicates */

		public boolean isNotRun() {
			return this == NOT_RUN;
		}

		public boolean isRunning() {
			return this == RUNNING || this == RUNNING_FAILURE
					|| this == RUNNING_ERROR;
		}

		public boolean isDone() {
			return this == OK || this == FAILURE || this == ERROR;
		}

		public static Status combineStatus(Status one, Status two) {
			Status progress = combineProgress(one, two);
			Status error = combineError(one, two);
			return combineProgressAndErrorStatus(progress, error);
		}

		private static Status combineProgress(Status one, Status two) {
			if (one.isNotRun() && two.isNotRun()) {
				return NOT_RUN;
			} else if (one.isDone() && two.isDone()) {
				return OK;
			} else if (!one.isRunning() && !two.isRunning()) {
				return OK; // one done, one not-run -> a parent failed and its
				// children are not run
			} else {
				return RUNNING;
			}
		}

		private static Status combineError(Status one, Status two) {
			if (one.isError() || two.isError()) {
				return ERROR;
			} else if (one.isFailure() || two.isFailure()) {
				return FAILURE;
			} else {
				return OK;
			}
		}

		private static Status combineProgressAndErrorStatus(Status progress,
				Status error) {
			if (progress.isDone()) {
				if (error.isError()) {
					return ERROR;
				}
				if (error.isFailure()) {
					return FAILURE;
				}
				return OK;
			}

			if (progress.isNotRun()) {
				// Assert.isTrue(!error.isErrorOrFailure());
				return NOT_RUN;
			}

			// Assert.isTrue(progress.isRunning());
			if (error.isError()) {
				return RUNNING_ERROR;
			}
			if (error.isFailure()) {
				return RUNNING_FAILURE;
			}
			// Assert.isTrue(error.isOK());
			return RUNNING;
		}

		/**
		 * @param oldStatus
		 *            one of {@link ITestRunListener2}'s STATUS_* constants
		 * @return the Status
		 */
		public static Status convert(int oldStatus) {
			return OLD_CODE[oldStatus];
		}

		public Result convertToResult() {
			if (isNotRun()) {
				return Result.UNDEFINED;
			}
			if (isError()) {
				return Result.ERROR;
			}
			if (isFailure()) {
				return Result.FAILURE;
			}
			if (isRunning()) {
				return Result.UNDEFINED;
			}
			return Result.OK;
		}

		public ProgressState convertToProgressState() {
			if (isRunning()) {
				return ProgressState.RUNNING;
			}
			if (isDone()) {
				return ProgressState.COMPLETED;
			}
			return ProgressState.NOT_STARTED;
		}

	}

	private final TestSuiteElement fParent;

	private final String fId;

	private String fTestName;

	private Status fStatus;

	private String fTrace;

	private String fExpected;

	private String fActual;

	/**
	 * @param parent
	 *            the parent, can be <code>null</code>
	 * @param id
	 *            the test id
	 * @param testName
	 *            the test name
	 */
	public TestElement(TestSuiteElement parent, String id, String testName) {
		Assert.isNotNull(id);
		Assert.isNotNull(testName);
		this.fParent = parent;
		this.fId = id;
		this.fTestName = testName;
		this.fStatus = Status.NOT_RUN;
		if (parent != null) {
			parent.addChild(this);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.gunit.ITestElement#getProgressState()
	 */
	public ProgressState getProgressState() {
		return getStatus().convertToProgressState();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.gunit.ITestElement#getTestResult()
	 */
	public Result getTestResult(boolean includeChildren) {
		return getStatus().convertToResult();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.gunit.ITestElement#getTestRunSession()
	 */
	public ITestRunSession getTestRunSession() {
		return getRoot().getTestRunSession();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.gunit.ITestElement#getParentContainer()
	 */
	public ITestElementContainer getParentContainer() {
		if (this.fParent instanceof TestRoot) {
			return getTestRunSession();
		}
		return this.fParent;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.gunit.model.ITestElement#getFailureTrace()
	 */
	public FailureTrace getFailureTrace() {
		Result testResult = getTestResult(false);
		if (testResult == Result.ERROR || testResult == Result.FAILURE) {
			return new FailureTrace(this.fTrace, this.fExpected, this.fActual);
		}
		return null;
	}

	/**
	 * @return the parent suite, or <code>null</code> for the root
	 */
	public TestSuiteElement getParent() {
		return this.fParent;
	}

	public String getId() {
		return this.fId;
	}

	public String getTestName() {
		return this.fTestName;
	}

	public void setName(String name) {
		this.fTestName = name;
	}

	public void setStatus(Status status) {
		// TODO: notify about change?
		// TODO: multiple errors/failures per test
		// https://bugs.eclipse.org/bugs/show_bug.cgi?id=125296

		this.fStatus = status;
		TestSuiteElement parent = getParent();
		if (parent != null) {
			parent.childChangedStatus(this, status);
		}
	}

	public void setStatus(Status status, String trace, String expected,
			String actual) {
		// TODO: notify about change?
		// TODO: multiple errors/failures per test
		// https://bugs.eclipse.org/bugs/show_bug.cgi?id=125296
		this.fTrace = trace;
		this.fExpected = expected;
		this.fActual = actual;
		setStatus(status);
	}

	public Status getStatus() {
		return this.fStatus;
	}

	public String getTrace() {
		return this.fTrace;
	}

	public String getExpected() {
		return this.fExpected;
	}

	public String getActual() {
		return this.fActual;
	}

	public boolean isComparisonFailure() {
		return this.fExpected != null && this.fActual != null;
	}

	/**
	 * @see org.erlide.gunit.internal.runner.ITestIdentifier#getName()
	 * @see org.erlide.gunit.internal.runner.MessageIds#TEST_IDENTIFIER_MESSAGE_FORMAT
	 */
	public String getClassName() {
		return extractClassName(getTestName());
	}

	private String extractClassName(String testNameString) {
		int index = testNameString.indexOf('(');
		if (index < 0) {
			return testNameString;
		}
		testNameString = testNameString.substring(index + 1);
		testNameString = testNameString.replace('$', '.'); // see bug 178503
		return testNameString.substring(0, testNameString.indexOf(')'));
	}

	public TestRoot getRoot() {
		return getParent().getRoot();
	}

	@Override
	public String toString() {
		return getProgressState() + " - " + getTestResult(true); //$NON-NLS-1$
	}
}
