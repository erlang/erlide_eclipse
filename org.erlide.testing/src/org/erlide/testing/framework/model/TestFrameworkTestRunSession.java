package org.erlide.testing.framework.model;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.erlide.testing.framework.launcher.TestFrameworkLaunchConfigurationConstants;


public class TestFrameworkTestRunSession extends Thread {

	private ILaunch fLaunch;
	// private BTErlRemoteTestRunnerClient fTestRunnerClient;
	private TestFrameworkInterface fBTErlInterface;
	private int fStartedCount;
	private int fEndedCount;
	private int fFailureCount;
	private int fTotalCount;
	// private long fStartTime;
	private ListenerList fSessionListeners;
	private List<TestFrameworkTestElement> fTestElements;

	public TestFrameworkTestRunSession(ILaunch launch) {
		fLaunch = launch;
		fBTErlInterface = new TestFrameworkInterface(fLaunch);

		init();

		fSessionListeners = new ListenerList();
	}

	@Override
	public void run() {

		for (int i = 0; i < fTestElements.size(); i++) {
			boolean hasFailingTestCases = false;
			TestFrameworkTestElement currentTestSuite = fTestElements.get(i);
			currentTestSuite.setStatus(TestFrameworkTestElement.STATUS_RUNNING);

			List<TestFrameworkTestElement> testCases = currentTestSuite.getChildren();
			for (int j = 0; j < testCases.size(); j++) {
				TestFrameworkTestElement currentTestCase = testCases.get(j);
				++fStartedCount;
				testStarted(currentTestCase);
				currentTestCase.setStatus(TestFrameworkTestElement.STATUS_RUNNING);
				int result = fBTErlInterface.runTest(currentTestCase);
				currentTestCase.setStatus(result);
				if (result == TestFrameworkTestElement.STATUS_FAILED) {
					++fFailureCount;
					testFailed(currentTestCase, "", "", "");
					hasFailingTestCases = true;
				}
				++fEndedCount;
				testEnded(currentTestCase);
			}

			if (hasFailingTestCases) {
				currentTestSuite.setStatus(TestFrameworkTestElement.STATUS_FAILED);
			} else {
				currentTestSuite.setStatus(TestFrameworkTestElement.STATUS_OK);
			}
		}

		fBTErlInterface.shutDownErlangNode();
	}

	private void init() {

		fStartedCount = 0;
		fEndedCount = 0;
		fFailureCount = 0;
		fTotalCount = 0;

		fTestElements = new ArrayList<TestFrameworkTestElement>();

		ILaunchConfiguration configuration = fLaunch.getLaunchConfiguration();

		try {
			String testSuiteName = configuration
					.getAttribute(
							TestFrameworkLaunchConfigurationConstants.ATTR_TEST_SUITE_NAME,
							"");

			if (testSuiteName.length() > 0) {
				TestFrameworkTestElement testElement = new TestFrameworkTestElement(
						testSuiteName, null);
				testElement.addChildren(fBTErlInterface
						.getTestCases(testElement));
				fTotalCount += testElement.getChildren().size();
				fTestElements.add(testElement);
			}
		} catch (CoreException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public void addTestSessionListener(ITestFrameworkTestSessionListener listener) {
		fSessionListeners.add(listener);
	}

	public void testRunStarted(int testCount) {
		// fIncompleteTestSuites= new ArrayList();

		fStartedCount = 0;
		fEndedCount = 0;
		// fIgnoredCount= 0;
		fFailureCount = 0;
		// fErrorCount= 0;
		fTotalCount = testCount;

		// fStartTime= System.currentTimeMillis();
		// fIsRunning= true;

		Object[] listeners = fSessionListeners.getListeners();
		for (int i = 0; i < listeners.length; ++i) {
			((ITestFrameworkTestSessionListener) listeners[i]).sessionStarted();
		}
	}

	public void testRunEnded(long elapsedTime) {

		Object[] listeners = fSessionListeners.getListeners();
		for (int i = 0; i < listeners.length; ++i) {
			((ITestFrameworkTestSessionListener) listeners[i])
					.sessionEnded(elapsedTime);
		}

	}

	public void testRunStopped(long elapsedTime) {
		Object[] listeners = fSessionListeners.getListeners();
		for (int i = 0; i < listeners.length; ++i) {
			((ITestFrameworkTestSessionListener) listeners[i])
					.sessionStopped(elapsedTime);
		}
	}

	public void testStarted(TestFrameworkTestElement testElement) {

		// if (fStartedCount == 0) {
		// Object[] listeners= fSessionListeners.getListeners();
		// for (int i= 0; i < listeners.length; ++i) {
		// ((IBTErlTestSessionListener) listeners[i]).runningBegins();
		// }
		// }
		// BTErlTestElement testElement = ((BTErlTestElement)
		// fTestElements.get(testId));
		// testElement.setStatus(BTErlTestElement.STATUS_RUNNING);
		//
		// if (!testElement.hasChildren()) {
		// fStartedCount++;
		// }
		//
		Object[] listeners = fSessionListeners.getListeners();
		for (int i = 0; i < listeners.length; ++i) {
			((ITestFrameworkTestSessionListener) listeners[i]).testStarted(testElement);
		}
	}

	public void testEnded(TestFrameworkTestElement testElement) {
		// BTErlTestElement testElement = ((BTErlTestElement)
		// fTestElements.get(testId));
		//
		// if (testElement.getStatus() == BTErlTestElement.STATUS_RUNNING) {
		// testElement.setStatus(BTErlTestElement.STATUS_OK);
		// }
		//
		// if (!testElement.hasChildren()) {
		// fEndedCount++;
		// }
		//
		Object[] listeners = fSessionListeners.getListeners();
		for (int i = 0; i < listeners.length; ++i) {
			((ITestFrameworkTestSessionListener) listeners[i]).testEnded(testElement);
		}
	}

	public void testRunTerminated() {
		Object[] listeners = fSessionListeners.getListeners();
		for (int i = 0; i < listeners.length; ++i) {
			((ITestFrameworkTestSessionListener) listeners[i]).sessionTerminated();
		}
	}

	public void testTreeEntry(String description) {
		TestFrameworkTestElement testElement = addTreeEntry(description);

		Object[] listeners = fSessionListeners.getListeners();
		for (int i = 0; i < listeners.length; ++i) {
			((ITestFrameworkTestSessionListener) listeners[i]).testAdded(testElement);
		}
	}

	private TestFrameworkTestElement addTreeEntry(String description) {
		// String[] elementInfo = description.split(",");
		// String testId = elementInfo[0];
		// String testName = elementInfo[1];
		// String parentTestId = elementInfo[2];
		// BTErlTestElement testElement;
		// if (parentTestId.equals("-")) {
		// testElement = new BTErlTestElement(testId, testName, null);
		// fTestElements.put(testId, testElement);
		// }
		// else {
		// BTErlTestElement parent = ((BTErlTestElement)
		// fTestElements.get(parentTestId));
		// testElement = new BTErlTestElement(testId, testName, parent);
		// parent.addChild(testElement);
		// fTestElements.put(testId, testElement);
		// }
		// return testElement;
		return null;
	}

	public void testFailed(TestFrameworkTestElement testElement, String trace,
			String expected, String actual) {
		// BTErlTestElement testElement = ((BTErlTestElement)
		// fTestElements.get(testId));
		//
		// testElement.setStatus(BTErlTestElement.STATUS_FAILED);
		//
		// fFailureCount++;
		//
		Object[] listeners = fSessionListeners.getListeners();
		for (int i = 0; i < listeners.length; ++i) {
			((ITestFrameworkTestSessionListener) listeners[i]).testFailed(testElement,
					trace, expected, actual);
		}
	}

	public List<TestFrameworkTestElement> getTestElements() {
		return fTestElements;
	}

	public int getTotalCount() {
		return fTotalCount;
	}

	public int getStartedCount() {
		return fStartedCount;
	}

	public int getEndedCount() {
		return fEndedCount;
	}

	public int getFailureCount() {
		return fFailureCount;
	}

	public ILaunch getLaunch() {
		return fLaunch;
	}

}
