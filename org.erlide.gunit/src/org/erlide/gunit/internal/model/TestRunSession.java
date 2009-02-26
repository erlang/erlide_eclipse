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

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.core.ILaunchesListener2;
import org.erlide.core.erlang.IErlProject;
import org.erlide.gunit.internal.Messages;
import org.erlide.gunit.internal.launcher.GUnitLaunchConfigurationConstants;
import org.erlide.gunit.internal.launcher.ITestKind;
import org.erlide.gunit.internal.model.TestElement.Status;
import org.erlide.gunit.internal.ui.GUnitPlugin;
import org.erlide.gunit.internal.ui.JUnitMessages;
import org.erlide.gunit.model.ITestElement;
import org.erlide.gunit.model.ITestElementContainer;
import org.erlide.gunit.model.ITestRunSession;

/**
 * A test run session holds all information about a test run, i.e. launch
 * configuration, launch, test tree (including results).
 */
public class TestRunSession implements ITestRunSession {

	/**
	 * The launch, or <code>null</code> iff this session was run externally.
	 */
	private final ILaunch fLaunch;
	private final String fTestRunName;
	/**
	 * Java project, or <code>null</code>.
	 */
	private final IErlProject fProject;

	private final ITestKind fTestRunnerKind;

	/**
	 * Test runner client or <code>null</code>.
	 */
	private RemoteTestRunnerClient fTestRunnerClient;

	private final ListenerList/* <ITestSessionListener> */fSessionListeners;

	/**
	 * The model root, or <code>null</code> if swapped to disk.
	 */
	private TestRoot fTestRoot;

	/**
	 * The test run session's cached result, or <code>null</code> if
	 * <code>fTestRoot != null</code>.
	 */
	private Result fTestResult;

	/**
	 * Map from testId to testElement.
	 */
	private HashMap<String, ITestElement> fIdToTest;

	/**
	 * The TestSuites for which additional children are expected.
	 */
	private List<IncompleteTestSuite> fIncompleteTestSuites;

	/**
	 * Suite for unrooted test case elements, or <code>null</code>.
	 */
	private TestSuiteElement fUnrootedSuite;

	/**
	 * Number of tests started during this test run.
	 */
	volatile int fStartedCount;
	/**
	 * Number of tests ignored during this test run.
	 */
	volatile int fIgnoredCount;
	/**
	 * Number of errors during this test run.
	 */
	volatile int fErrorCount;
	/**
	 * Number of failures during this test run.
	 */
	volatile int fFailureCount;
	/**
	 * Total number of tests to run.
	 */
	volatile int fTotalCount;
	/**
	 * Start time in millis.
	 */
	volatile long fStartTime;
	volatile boolean fIsRunning;

	volatile boolean fIsStopped;

	/**
	 * @param testRunName
	 * @param project
	 *            may be <code>null</code>
	 */
	public TestRunSession(String testRunName, IErlProject project) {
		// TODO: check assumptions about non-null fields

		fLaunch = null;
		fProject = null; // TODO

		Assert.isNotNull(testRunName);
		fTestRunName = testRunName;
		fTestRunnerKind = ITestKind.NULL; // TODO

		fTestRoot = new TestRoot(this);
		fIdToTest = new HashMap();

		fTestRunnerClient = null;

		fSessionListeners = new ListenerList();
	}

	public TestRunSession(ILaunch launch, IErlProject project, int port) {
		Assert.isNotNull(launch);

		fLaunch = launch;
		fProject = project;

		ILaunchConfiguration launchConfiguration = launch
				.getLaunchConfiguration();
		if (launchConfiguration != null) {
			fTestRunName = launchConfiguration.getName();
			fTestRunnerKind = GUnitLaunchConfigurationConstants
					.getTestRunnerKind(launchConfiguration);
		} else {
			fTestRunName = project.getName();
			fTestRunnerKind = ITestKind.NULL;
		}

		fTestRoot = new TestRoot(this);
		fIdToTest = new HashMap();

		fTestRunnerClient = new RemoteTestRunnerClient();
		fTestRunnerClient.startListening(
				new ITestRunListener2[] { new TestSessionNotifier() }, port);

		final ILaunchManager launchManager = DebugPlugin.getDefault()
				.getLaunchManager();
		launchManager.addLaunchListener(new ILaunchesListener2() {
			public void launchesTerminated(ILaunch[] launches) {
				if (Arrays.asList(launches).contains(fLaunch)) {
					if (fTestRunnerClient != null) {
						fTestRunnerClient.stopWaiting();
					}
					launchManager.removeLaunchListener(this);
				}
			}

			public void launchesRemoved(ILaunch[] launches) {
				if (Arrays.asList(launches).contains(fLaunch)) {
					if (fTestRunnerClient != null) {
						fTestRunnerClient.stopWaiting();
					}
					launchManager.removeLaunchListener(this);
				}
			}

			public void launchesChanged(ILaunch[] launches) {
			}

			public void launchesAdded(ILaunch[] launches) {
			}
		});

		fSessionListeners = new ListenerList();
		addTestSessionListener(new TestRunListenerAdapter(this));
	}

	void reset() {
		fStartedCount = 0;
		fFailureCount = 0;
		fErrorCount = 0;
		fIgnoredCount = 0;
		fTotalCount = 0;

		fTestRoot = new TestRoot(this);
		fTestResult = null;
		fIdToTest = new HashMap();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.gunit.ITestRunSession#getProgressState()
	 */
	public ProgressState getProgressState() {
		if (isRunning()) {
			return ProgressState.RUNNING;
		}
		if (isStopped()) {
			return ProgressState.STOPPED;
		}
		return ProgressState.COMPLETED;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.gunit.model.ITestElement#getTestResult(boolean)
	 */
	public Result getTestResult(boolean includeChildren) {
		if (fTestRoot != null) {
			return fTestRoot.getTestResult(true);
		} else {
			return fTestResult;
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.gunit.model.ITestElementContainer#getChildren()
	 */
	public ITestElement[] getChildren() {
		return getTestRoot().getChildren();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.gunit.model.ITestElement#getFailureTrace()
	 */
	public FailureTrace getFailureTrace() {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.gunit.model.ITestElement#getParentContainer()
	 */
	public ITestElementContainer getParentContainer() {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.gunit.model.ITestElement#getTestRunSession()
	 */
	public ITestRunSession getTestRunSession() {
		return this;
	}

	public TestRoot getTestRoot() {
		swapIn(); // TODO: TestRoot should stay (e.g. for
		// getTestRoot().getStatus())
		return fTestRoot;
	}

	/**
	 * @return the Java project, or <code>null</code>
	 */
	public IErlProject getLaunchedProject() {
		return fProject;
	}

	public ITestKind getTestRunnerKind() {
		return fTestRunnerKind;
	}

	/**
	 * @return the launch, or <code>null</code> iff this session was run
	 *         externally
	 */
	public ILaunch getLaunch() {
		return fLaunch;
	}

	public String getTestRunName() {
		return fTestRunName;
	}

	public int getErrorCount() {
		return fErrorCount;
	}

	public int getFailureCount() {
		return fFailureCount;
	}

	public int getStartedCount() {
		return fStartedCount;
	}

	public int getIgnoredCount() {
		return fIgnoredCount;
	}

	public int getTotalCount() {
		return fTotalCount;
	}

	public long getStartTime() {
		return fStartTime;
	}

	/**
	 * @return <code>true</code> iff the session has been stopped or terminated
	 */
	public boolean isStopped() {
		return fIsStopped;
	}

	public void addTestSessionListener(ITestSessionListener listener) {
		swapIn();
		fSessionListeners.add(listener);
	}

	public void removeTestSessionListener(ITestSessionListener listener) {
		fSessionListeners.remove(listener);
	}

	public void swapOut() {
		if (fTestRoot == null) {
			return;
		}
		if (isRunning() || isStarting() || isKeptAlive()) {
			return;
		}

		Object[] listeners = fSessionListeners.getListeners();
		for (int i = 0; i < listeners.length; ++i) {
			ITestSessionListener registered = (ITestSessionListener) listeners[i];
			if (!registered.acceptsSwapToDisk()) {
				return;
			}
		}

		try {
			File swapFile = getSwapFile();

			GUnitModel.exportTestRunSession(this, swapFile);
			fTestResult = fTestRoot.getTestResult(true);
			fTestRoot = null;
			fTestRunnerClient = null;
			fIdToTest = new HashMap();
			fIncompleteTestSuites = null;
			fUnrootedSuite = null;

		} catch (IllegalStateException e) {
			GUnitPlugin.log(e);
		} catch (CoreException e) {
			GUnitPlugin.log(e);
		}
	}

	private boolean isStarting() {
		return getStartTime() == 0 && fLaunch != null
				&& !fLaunch.isTerminated();
	}

	public void removeSwapFile() {
		File swapFile = getSwapFile();
		if (swapFile.exists()) {
			swapFile.delete();
		}
	}

	private File getSwapFile() throws IllegalStateException {
		File historyDir = GUnitPlugin.getHistoryDirectory();
		String isoTime = new SimpleDateFormat("yyyyMMdd-HHmmss.SSS").format(new Date(getStartTime())); //$NON-NLS-1$
		String swapFileName = isoTime + ".xml"; //$NON-NLS-1$
		return new File(historyDir, swapFileName);
	}

	public void swapIn() {
		if (fTestRoot != null) {
			return;
		}

		try {
			GUnitModel.importIntoTestRunSession(getSwapFile(), this);
		} catch (IllegalStateException e) {
			GUnitPlugin.log(e);
			fTestRoot = new TestRoot(this);
			fTestResult = null;
		} catch (CoreException e) {
			GUnitPlugin.log(e);
			fTestRoot = new TestRoot(this);
			fTestResult = null;
		}
	}

	public void stopTestRun() {
		if (isRunning() || !isKeptAlive()) {
			fIsStopped = true;
		}
		if (fTestRunnerClient != null) {
			fTestRunnerClient.stopTest();
		}
	}

	/**
	 * @return <code>true</code> iff the runtime VM of this test session is
	 *         still alive
	 */
	public boolean isKeptAlive() {
		if (fTestRunnerClient != null && fLaunch != null
				&& fTestRunnerClient.isRunning()
				&& ILaunchManager.DEBUG_MODE.equals(fLaunch.getLaunchMode())) {
			ILaunchConfiguration config = fLaunch.getLaunchConfiguration();
			try {
				return config != null
						&& config
								.getAttribute(
										GUnitLaunchConfigurationConstants.ATTR_KEEPRUNNING,
										false);
			} catch (CoreException e) {
				return false;
			}

		} else {
			return false;
		}
	}

	/**
	 * @return <code>true</code> iff this session has been started, but not
	 *         ended nor stopped nor terminated
	 */
	public boolean isRunning() {
		return fIsRunning;
	}

	/**
	 * @param testId
	 * @param className
	 * @param testName
	 * @param launchMode
	 * @return <code>false</code> iff the rerun could not be started
	 * @throws CoreException
	 */
	public boolean rerunTest(String testId, String className, String testName,
			String launchMode) throws CoreException {
		if (isKeptAlive()) {
			Status status = ((TestCaseElement) getTestElement(testId))
					.getStatus();
			if (status == Status.ERROR) {
				fErrorCount--;
			} else if (status == Status.FAILURE) {
				fFailureCount--;
			}
			fTestRunnerClient.rerunTest(testId, className, testName);
			return true;

		} else if (fLaunch != null) {
			// run the selected test using the previous launch configuration
			ILaunchConfiguration launchConfiguration = fLaunch
					.getLaunchConfiguration();
			if (launchConfiguration != null) {

				String name = className;
				if (testName != null) {
					name += "." + testName; //$NON-NLS-1$
				}
				String configName = Messages.format(
						JUnitMessages.TestRunnerViewPart_configName, name);
				ILaunchConfigurationWorkingCopy tmp = launchConfiguration
						.copy(configName);
				// fix for bug: 64838 junit view run single test does not use
				// correct class [JUnit]
				// tmp.setAttribute(
				// IJavaLaunchConfigurationConstants.ATTR_MAIN_TYPE_NAME,
				// className);

				// reset the container
				tmp.setAttribute(
						GUnitLaunchConfigurationConstants.ATTR_TEST_CONTAINER,
						""); //$NON-NLS-1$
				if (testName != null) {
					tmp
							.setAttribute(
									GUnitLaunchConfigurationConstants.ATTR_TEST_METHOD_NAME,
									testName);
					// String args= "-rerun "+testId;
					// tmp.setAttribute(IJavaLaunchConfigurationConstants.ATTR_PROGRAM_ARGUMENTS,
					// args);
				}
				tmp.launch(launchMode, null);
				return true;
			}
		}

		return false;
	}

	public TestElement getTestElement(String id) {
		return (TestElement) fIdToTest.get(id);
	}

	private TestElement addTreeEntry(String treeEntry) {
		// format: testId","testName","isSuite","testcount
		int index0 = treeEntry.indexOf(',');
		String id = treeEntry.substring(0, index0);

		StringBuffer testNameBuffer = new StringBuffer(100);
		int index1 = scanTestName(treeEntry, index0 + 1, testNameBuffer);
		String testName = testNameBuffer.toString().trim();

		int index2 = treeEntry.indexOf(',', index1 + 1);
		boolean isSuite = treeEntry.substring(index1 + 1, index2)
				.equals("true"); //$NON-NLS-1$

		int testCount = Integer.parseInt(treeEntry.substring(index2 + 1));

		if (fIncompleteTestSuites.isEmpty()) {
			return createTestElement(fTestRoot, id, testName, isSuite,
					testCount);
		} else {
			int suiteIndex = fIncompleteTestSuites.size() - 1;
			IncompleteTestSuite openSuite = fIncompleteTestSuites
					.get(suiteIndex);
			openSuite.fOutstandingChildren--;
			if (openSuite.fOutstandingChildren <= 0) {
				fIncompleteTestSuites.remove(suiteIndex);
			}
			return createTestElement(openSuite.fTestSuiteElement, id, testName,
					isSuite, testCount);
		}
	}

	public TestElement createTestElement(TestSuiteElement parent, String id,
			String testName, boolean isSuite, int testCount) {
		TestElement testElement;
		if (isSuite) {
			TestSuiteElement testSuiteElement = new TestSuiteElement(parent,
					id, testName, testCount);
			testElement = testSuiteElement;
			if (testCount > 0) {
				fIncompleteTestSuites.add(new IncompleteTestSuite(
						testSuiteElement, testCount));
			}
		} else {
			testElement = new TestCaseElement(parent, id, testName);
		}
		fIdToTest.put(id, testElement);
		return testElement;
	}

	/**
	 * Append the test name from <code>s</code> to <code>testName</code>.
	 * 
	 * @param s
	 *            the string to scan
	 * @param start
	 *            the offset of the first character in <code>s</code>
	 * @param testName
	 *            the result
	 * 
	 * @return the index of the next ','
	 */
	private int scanTestName(String s, int start, StringBuffer testName) {
		boolean inQuote = false;
		int i = start;
		for (; i < s.length(); i++) {
			char c = s.charAt(i);
			if (c == '\\' && !inQuote) {
				inQuote = true;
				continue;
			} else if (inQuote) {
				inQuote = false;
				testName.append(c);
			} else if (c == ',') {
				break;
			} else {
				testName.append(c);
			}
		}
		return i;
	}

	/**
	 * An {@link ITestRunListener2} that listens to events from the
	 * {@link RemoteTestRunnerClient} and translates them into high-level model
	 * events (broadcasted to {@link ITestSessionListener}s).
	 */
	private class TestSessionNotifier implements ITestRunListener2 {

		public void testRunStarted(int testCount) {
			fIncompleteTestSuites = new ArrayList();

			fStartedCount = 0;
			fIgnoredCount = 0;
			fFailureCount = 0;
			fErrorCount = 0;
			fTotalCount = testCount;

			fStartTime = System.currentTimeMillis();
			fIsRunning = true;

			Object[] listeners = fSessionListeners.getListeners();
			for (int i = 0; i < listeners.length; ++i) {
				((ITestSessionListener) listeners[i]).sessionStarted();
			}
		}

		public void testRunEnded(long elapsedTime) {
			fIsRunning = false;

			Object[] listeners = fSessionListeners.getListeners();
			for (int i = 0; i < listeners.length; ++i) {
				((ITestSessionListener) listeners[i]).sessionEnded(elapsedTime);
			}
		}

		public void testRunStopped(long elapsedTime) {
			fIsRunning = false;
			fIsStopped = true;

			Object[] listeners = fSessionListeners.getListeners();
			for (int i = 0; i < listeners.length; ++i) {
				((ITestSessionListener) listeners[i])
						.sessionStopped(elapsedTime);
			}
		}

		public void testRunTerminated() {
			fIsRunning = false;
			fIsStopped = true;

			Object[] listeners = fSessionListeners.getListeners();
			for (int i = 0; i < listeners.length; ++i) {
				((ITestSessionListener) listeners[i]).sessionTerminated();
			}
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.erlide.gunit.internal.model.ITestRunListener2#testTreeEntry(java
		 * .lang.String)
		 */
		public void testTreeEntry(String description) {
			TestElement testElement = addTreeEntry(description);

			Object[] listeners = fSessionListeners.getListeners();
			for (int i = 0; i < listeners.length; ++i) {
				((ITestSessionListener) listeners[i]).testAdded(testElement);
			}
		}

		private TestElement createUnrootedTestElement(String testId,
				String testName) {
			TestSuiteElement unrootedSuite = getUnrootedSuite();
			TestElement testElement = createTestElement(unrootedSuite, testId,
					testName, false, 1);

			Object[] listeners = fSessionListeners.getListeners();
			for (int i = 0; i < listeners.length; ++i) {
				((ITestSessionListener) listeners[i]).testAdded(testElement);
			}

			return testElement;
		}

		private TestSuiteElement getUnrootedSuite() {
			if (fUnrootedSuite == null) {
				fUnrootedSuite = (TestSuiteElement) createTestElement(
						fTestRoot,
						"-2", JUnitMessages.TestRunSession_unrootedTests, true, 0); //$NON-NLS-1$
			}
			return fUnrootedSuite;
		}

		public void testStarted(String testId, String testName) {
			if (fStartedCount == 0) {
				Object[] listeners = fSessionListeners.getListeners();
				for (int i = 0; i < listeners.length; ++i) {
					((ITestSessionListener) listeners[i]).runningBegins();
				}
			}
			TestElement testElement = getTestElement(testId);
			if (testElement == null) {
				testElement = createUnrootedTestElement(testId, testName);
			} else if (!(testElement instanceof TestCaseElement)) {
				logUnexpectedTest(testId, testElement);
				return;
			}
			TestCaseElement testCaseElement = (TestCaseElement) testElement;
			setStatus(testCaseElement, Status.RUNNING);

			fStartedCount++;

			Object[] listeners = fSessionListeners.getListeners();
			for (int i = 0; i < listeners.length; ++i) {
				((ITestSessionListener) listeners[i])
						.testStarted(testCaseElement);
			}
		}

		public void testEnded(String testId, String testName) {
			TestElement testElement = getTestElement(testId);
			if (testElement == null) {
				testElement = createUnrootedTestElement(testId, testName);
			} else if (!(testElement instanceof TestCaseElement)) {
				logUnexpectedTest(testId, testElement);
				return;
			}
			TestCaseElement testCaseElement = (TestCaseElement) testElement;
			if (testName.startsWith(MessageIds.IGNORED_TEST_PREFIX)) {
				testCaseElement.setIgnored(true);
				fIgnoredCount++;
			}

			if (testCaseElement.getStatus() == Status.RUNNING) {
				setStatus(testCaseElement, Status.OK);
			}

			Object[] listeners = fSessionListeners.getListeners();
			for (int i = 0; i < listeners.length; ++i) {
				((ITestSessionListener) listeners[i])
						.testEnded(testCaseElement);
			}
		}

		public void testFailed(int status, String testId, String testName,
				String trace) {
			testFailed(status, testId, testName, trace, null, null);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.erlide.gunit.internal.model.ITestRunListener2#testFailed(int,
		 * java.lang.String, java.lang.String, java.lang.String,
		 * java.lang.String, java.lang.String)
		 */
		public void testFailed(int statusCode, String testId, String testName,
				String trace, String expected, String actual) {
			TestElement testElement = getTestElement(testId);
			if (testElement == null) {
				testElement = createUnrootedTestElement(testId, testName);
				return;
			}

			Status status = Status.convert(statusCode);
			registerTestFailed(testElement, status, trace,
					nullifyEmpty(expected), nullifyEmpty(actual));

			Object[] listeners = fSessionListeners.getListeners();
			for (int i = 0; i < listeners.length; ++i) {
				((ITestSessionListener) listeners[i]).testFailed(testElement,
						status, trace, expected, actual);
			}
		}

		private String nullifyEmpty(String string) {
			int length = string.length();
			if (length == 0) {
				return null;
			} else if (string.charAt(length - 1) == '\n') {
				return string.substring(0, length - 1);
			} else {
				return string;
			}
		}

		public void testReran(String testId, String testClass, String testName,
				int status, String trace) {
			testReran(testId, testClass, testName, status, trace, "", ""); //$NON-NLS-1$ //$NON-NLS-2$
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see
		 * org.erlide.gunit.internal.model.ITestRunListener2#testReran(java.
		 * lang.String, java.lang.String, java.lang.String, int,
		 * java.lang.String, java.lang.String, java.lang.String)
		 */
		public void testReran(String testId, String className, String testName,
				int statusCode, String trace, String expectedResult,
				String actualResult) {
			TestElement testElement = getTestElement(testId);
			if (testElement == null) {
				testElement = createUnrootedTestElement(testId, testName);
			} else if (!(testElement instanceof TestCaseElement)) {
				logUnexpectedTest(testId, testElement);
				return;
			}
			TestCaseElement testCaseElement = (TestCaseElement) testElement;

			Status status = Status.convert(statusCode);
			registerTestFailed(testElement, status, trace,
					nullifyEmpty(expectedResult), nullifyEmpty(actualResult));

			Object[] listeners = fSessionListeners.getListeners();
			for (int i = 0; i < listeners.length; ++i) {
				// TODO: post old & new status?
				((ITestSessionListener) listeners[i]).testReran(
						testCaseElement, status, trace, expectedResult,
						actualResult);
			}
		}

		private void logUnexpectedTest(String testId, TestElement testElement) {
			GUnitPlugin
					.log(new Exception(
							"Unexpected TestElement type for testId '" + testId + "': " + testElement)); //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	private static class IncompleteTestSuite {
		public TestSuiteElement fTestSuiteElement;
		public int fOutstandingChildren;

		public IncompleteTestSuite(TestSuiteElement testSuiteElement,
				int outstandingChildren) {
			fTestSuiteElement = testSuiteElement;
			fOutstandingChildren = outstandingChildren;
		}
	}

	public void registerTestFailed(TestElement testElement, Status status,
			String trace, String expected, String actual) {
		testElement.setStatus(status, trace, expected, actual);
		if (status.isError()) {
			fErrorCount++;
		} else {
			fFailureCount++;
		}
	}

	public void registerTestEnded(TestElement testElement, boolean completed) {
		if (testElement instanceof TestCaseElement) {
			fTotalCount++;
			if (!completed) {
				return;
			}
			fStartedCount++;
			if (((TestCaseElement) testElement).isIgnored()) {
				fIgnoredCount++;
			}
			if (!testElement.getStatus().isErrorOrFailure()) {
				setStatus(testElement, Status.OK);
			}
		}
	}

	private void setStatus(TestElement testElement, Status status) {
		testElement.setStatus(status);
	}

	public TestElement[] getAllFailedTestElements() {
		ArrayList failures = new ArrayList();
		addFailures(failures, getTestRoot());
		return (TestElement[]) failures
				.toArray(new TestElement[failures.size()]);
	}

	private void addFailures(ArrayList failures, ITestElement testElement) {
		Result testResult = testElement.getTestResult(true);
		if (testResult == Result.ERROR || testResult == Result.FAILURE) {
			failures.add(testElement);
		}
		if (testElement instanceof TestSuiteElement) {
			TestSuiteElement testSuiteElement = (TestSuiteElement) testElement;
			ITestElement[] children = testSuiteElement.getChildren();
			for (int i = 0; i < children.length; i++) {
				addFailures(failures, children[i]);
			}
		}
	}

}
