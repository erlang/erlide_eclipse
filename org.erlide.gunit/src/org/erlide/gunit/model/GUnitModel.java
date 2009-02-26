package org.erlide.gunit.model;

import java.util.LinkedList;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.ListenerList;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.erlide.gunit.GUnitPlugin;
import org.erlide.gunit.internal.ui.TestRunnerViewPart;

public class GUnitModel {

	// private final class BTErlLaunchListener implements ILaunchListener {
	//
	// /**
	// * Used to track new launches. We need to do this
	// * so that we only attach a TestRunner once to a launch.
	// * Once a test runner is connected, it is removed from the set.
	// */
	// private HashSet fTrackedLaunches= new HashSet(20);
	//
	// /*
	// * @see ILaunchListener#launchAdded(ILaunch)
	// */
	// public void launchAdded(ILaunch launch) {
	// System.out.println("launchAdded");
	// fTrackedLaunches.add(launch);
	// }
	//
	// /*
	// * @see ILaunchListener#launchRemoved(ILaunch)
	// */
	// public void launchRemoved(final ILaunch launch) {
	// System.out.println("launchRemoved");
	// fTrackedLaunches.remove(launch);
	// //TODO: story for removing old test runs?
	// }
	//
	// /*
	// * @see ILaunchListener#launchChanged(ILaunch)
	// */
	// public void launchChanged(final ILaunch launch) {
	// System.out.println("launchChanged");
	// if (!fTrackedLaunches.contains(launch))
	// return;
	//
	// ILaunchConfiguration config= launch.getLaunchConfiguration();
	// if (config == null)
	// return;
	//
	// // final IJavaProject javaProject=
	// JUnitLaunchConfigurationConstants.getJavaProject(config);
	// // if (javaProject == null)
	// // return;
	// //
	// // // test whether the launch defines the JUnit attributes
	// // String portStr=
	// launch.getAttribute(JUnitLaunchConfigurationConstants.ATTR_PORT);
	// // if (portStr == null)
	// // return;
	//
	// // try {
	// // final int port= Integer.parseInt(portStr);
	// fTrackedLaunches.remove(launch);
	// getDisplay().asyncExec(new Runnable() {
	// public void run() {
	// // connectTestRunner(launch, javaProject, port);
	// connectTestRunner(launch);
	// }
	// });
	// // } catch (NumberFormatException e) {
	// // return;
	// // }
	// }
	//
	// private void connectTestRunner(ILaunch launch) {
	// showTestRunnerViewPartInActivePage(findTestRunnerViewPartInActivePage());
	//
	// //TODO: Do notifications have to be sent in UI thread?
	// // Check concurrent access to fTestRunSessions (no problem inside
	// asyncExec())
	//
	// //SHADE: We only support one test run at a time (for now)
	//
	// // int maxCount=
	// JUnitPlugin.getDefault().getPreferenceStore().getInt(JUnitPreferencesConstants.MAX_TEST_RUNS);
	// // int toDelete= fTestRunSessions.size() - maxCount;
	// // while (toDelete > 0) {
	// // toDelete--;
	// // TestRunSession session= (TestRunSession)
	// fTestRunSessions.removeLast();
	// // notifyTestRunSessionRemoved(session);
	// // }
	//
	// BTErlTestRunSession testRunSession= new BTErlTestRunSession(launch);
	// addTestRunSession(testRunSession);
	// testRunSession.start();
	// }
	//
	// private BTErlTestRunnerViewPart
	// showTestRunnerViewPartInActivePage(BTErlTestRunnerViewPart testRunner) {
	// IWorkbenchPart activePart= null;
	// IWorkbenchPage page= null;
	// try {
	// // TODO: have to force the creation of view part contents
	// // otherwise the UI will not be updated
	// if (testRunner != null && testRunner.isCreated())
	// return testRunner;
	// page= BTErlPlugin.getActivePage();
	// if (page == null)
	// return null;
	// activePart= page.getActivePart();
	// // show the result view if it isn't shown yet
	// return (BTErlTestRunnerViewPart)
	// page.showView(BTErlTestRunnerViewPart.VIEW_ID);
	// } catch (PartInitException pie) {
	// pie.printStackTrace();
	// return null;
	// } finally{
	// //restore focus stolen by the creation of the result view
	// if (page != null && activePart != null)
	// page.activate(activePart);
	// }
	// }
	//
	// private BTErlTestRunnerViewPart findTestRunnerViewPartInActivePage() {
	// IWorkbenchPage page= BTErlPlugin.getActivePage();
	// if (page == null)
	// return null;
	// return (BTErlTestRunnerViewPart)
	// page.findView(BTErlTestRunnerViewPart.VIEW_ID);
	// }
	//
	// private Display getDisplay() {
	// Display display= Display.getCurrent();
	// if (display == null) {
	// display= Display.getDefault();
	// }
	// return display;
	// }
	// }

	private final ListenerList fTestRunSessionListeners = new ListenerList();

	/**
	 * Active test run sessions, youngest first.
	 */
	private final LinkedList<TestRunSession> fTestRunSessions = new LinkedList<TestRunSession>();

	// private final ILaunchListener fLaunchListener= new BTErlLaunchListener();

	public void runTests(ILaunch launch) {
		getDisplay().asyncExec(new Runnable() {
			public void run() {
				showTestRunnerViewPartInActivePage(findTestRunnerViewPartInActivePage());
			}
		});

		TestRunSession testRunSession = new TestRunSession(launch);
		addTestRunSession(testRunSession);
		testRunSession.start();
	}

	// public void start() {
	// ILaunchManager launchManager=
	// DebugPlugin.getDefault().getLaunchManager();
	// launchManager.addLaunchListener(fLaunchListener);
	// }
	//
	// public void stop() {
	// ILaunchManager launchManager=
	// DebugPlugin.getDefault().getLaunchManager();
	// launchManager.removeLaunchListener(fLaunchListener);
	// }

	public void addBTErlTestRunSessionListener(ITestRunSessionListener listener) {
		fTestRunSessionListeners.add(listener);
	}

	public void removeBTErlTestRunSessionListener(
			ITestRunSessionListener listener) {
		fTestRunSessionListeners.remove(listener);
	}

	/**
	 * Adds the given {@link TestRunSession} and notifies all registered
	 * {@link ITestRunSessionListener}s.
	 * <p>
	 * <b>To be called in the UI thread only!</b>
	 * </p>
	 * 
	 * @param testRunSession
	 *            the session to add
	 */
	public void addTestRunSession(TestRunSession testRunSession) {
		Assert.isNotNull(testRunSession);
		Assert.isLegal(!fTestRunSessions.contains(testRunSession));
		fTestRunSessions.addFirst(testRunSession);
		notifyTestRunSessionAdded(testRunSession);
	}

	private void notifyTestRunSessionAdded(TestRunSession testRunSession) {
		Object[] listeners = fTestRunSessionListeners.getListeners();
		for (int i = 0; i < listeners.length; ++i) {
			((ITestRunSessionListener) listeners[i])
					.sessionAdded(testRunSession);
		}
	}

	TestRunnerViewPart showTestRunnerViewPartInActivePage(
			TestRunnerViewPart testRunner) {
		IWorkbenchPart activePart = null;
		IWorkbenchPage page = null;
		try {
			// TODO: have to force the creation of view part contents
			// otherwise the UI will not be updated
			if (testRunner != null && testRunner.isCreated()) {
				return testRunner;
			}
			page = GUnitPlugin.getActivePage();
			if (page == null) {
				return null;
			}
			activePart = page.getActivePart();
			// show the result view if it isn't shown yet
			return (TestRunnerViewPart) page
					.showView(TestRunnerViewPart.VIEW_ID);
		} catch (PartInitException pie) {
			pie.printStackTrace();
			return null;
		} finally {
			// restore focus stolen by the creation of the result view
			if (page != null && activePart != null) {
				page.activate(activePart);
			}
		}
	}

	TestRunnerViewPart findTestRunnerViewPartInActivePage() {
		IWorkbenchPage page = GUnitPlugin.getActivePage();
		if (page == null) {
			return null;
		}
		return (TestRunnerViewPart) page.findView(TestRunnerViewPart.VIEW_ID);
	}

	private Display getDisplay() {
		Display display = Display.getCurrent();
		if (display == null) {
			display = Display.getDefault();
		}
		return display;
	}
}
