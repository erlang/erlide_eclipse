/*******************************************************************************
 * Copyright (c) 2000, 2007 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Julien Ruaux: jruaux@octo.com see bug 25324 Ability to know when tests are finished [junit]
 *     Vincent Massol: vmassol@octo.com 25324 Ability to know when tests are finished [junit]
 *     Sebastian Davids: sdavids@gmx.de 35762 JUnit View wasting a lot of screen space [JUnit]
 *******************************************************************************/
package org.erlide.gunit.internal.ui;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.MessageFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ILock;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.ViewForm;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.ControlListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorActionBarContributor;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.handlers.IHandlerActivation;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.part.EditorActionBarContributor;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.IWorkbenchSiteProgressService;
import org.eclipse.ui.progress.UIJob;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.util.IElementChangedListener;
import org.erlide.gunit.internal.Messages;
import org.erlide.gunit.internal.launcher.GUnitLaunchConfigurationConstants;
import org.erlide.gunit.internal.launcher.ITestKind;
import org.erlide.gunit.internal.model.GUnitModel;
import org.erlide.gunit.internal.model.ITestRunSessionListener;
import org.erlide.gunit.internal.model.ITestSessionListener;
import org.erlide.gunit.internal.model.TestCaseElement;
import org.erlide.gunit.internal.model.TestElement;
import org.erlide.gunit.internal.model.TestRunSession;
import org.erlide.gunit.model.ITestElement.Result;

/**
 * A ViewPart that shows the results of a test run.
 */
public class TestRunnerViewPart extends ViewPart {

	public static final String NAME = "org.erlide.gunit.ResultView"; //$NON-NLS-1$

	private static final String RERUN_LAST_COMMAND = "org.erlide.gunit.junitShortcut.rerunLast"; //$NON-NLS-1$

	private static final String RERUN_FAILED_FIRST_COMMAND = "org.erlide.gunit.junitShortcut.rerunFailedFirst"; //$NON-NLS-1$

	static final int REFRESH_INTERVAL = 200;

	static final int LAYOUT_FLAT = 0;

	static final int LAYOUT_HIERARCHICAL = 1;

	/**
	 * Whether the output scrolls and reveals tests as they are executed.
	 */
	protected boolean fAutoScroll = true;

	/**
	 * The current orientation; either <code>VIEW_ORIENTATION_HORIZONTAL</code>
	 * <code>VIEW_ORIENTATION_VERTICAL</code>, or
	 * <code>VIEW_ORIENTATION_AUTOMATIC</code>.
	 */
	private int fOrientation = VIEW_ORIENTATION_AUTOMATIC;

	/**
	 * The current orientation; either <code>VIEW_ORIENTATION_HORIZONTAL</code>
	 * <code>VIEW_ORIENTATION_VERTICAL</code>.
	 */
	private int fCurrentOrientation;

	/**
	 * The current layout mode (LAYOUT_FLAT or LAYOUT_HIERARCHICAL).
	 */
	private int fLayout = LAYOUT_HIERARCHICAL;

	// private boolean fTestIsRunning= false;

	protected GUnitProgressBar fProgressBar;

	protected ProgressImages fProgressImages;

	protected Image fViewImage;

	protected CounterPanel fCounterPanel;

	protected boolean fShowOnErrorOnly = false;

	protected Clipboard fClipboard;

	protected volatile String fInfoMessage;

	private FailureTrace fFailureTrace;

	private TestViewer fTestViewer;

	/**
	 * Is the UI disposed?
	 */
	private boolean fIsDisposed = false;

	/**
	 * Actions
	 */
	private Action fNextAction;

	private Action fPreviousAction;

	private StopAction fStopAction;

	private GUnitCopyAction fCopyAction;

	private Action fRerunLastTestAction;

	private IHandlerActivation fRerunLastActivation;

	private Action fRerunFailedFirstAction;

	private IHandlerActivation fRerunFailedFirstActivation;

	private Action fFailuresOnlyFilterAction;

	private ScrollLockAction fScrollLockAction;

	private ToggleOrientationAction[] fToggleOrientationActions;

	private ShowTestHierarchyAction fShowTestHierarchyAction;

	private ActivateOnErrorAction fActivateOnErrorAction;

	private IMenuListener fViewMenuListener;

	private TestRunSession fTestRunSession;

	private TestSessionListener fTestSessionListener;

	private RunnerViewHistory fViewHistory;

	private TestRunSessionListener fTestRunSessionListener;

	final Image fStackViewIcon = TestRunnerViewPart
	.createImage("eview16/stackframe.gif");//$NON-NLS-1$

	final Image fTestRunOKIcon = TestRunnerViewPart
	.createImage("eview16/junitsucc.gif"); //$NON-NLS-1$

	final Image fTestRunFailIcon = TestRunnerViewPart
	.createImage("eview16/juniterr.gif"); //$NON-NLS-1$

	final Image fTestRunOKDirtyIcon = TestRunnerViewPart
	.createImage("eview16/junitsuccq.gif"); //$NON-NLS-1$

	final Image fTestRunFailDirtyIcon = TestRunnerViewPart
	.createImage("eview16/juniterrq.gif"); //$NON-NLS-1$

	final Image fTestIcon = TestRunnerViewPart.createImage("obj16/test.gif"); //$NON-NLS-1$

	final Image fTestOkIcon = TestRunnerViewPart
	.createImage("obj16/testok.gif"); //$NON-NLS-1$

	final Image fTestErrorIcon = TestRunnerViewPart
	.createImage("obj16/testerr.gif"); //$NON-NLS-1$

	final Image fTestFailIcon = TestRunnerViewPart
	.createImage("obj16/testfail.gif"); //$NON-NLS-1$

	final Image fTestRunningIcon = TestRunnerViewPart
	.createImage("obj16/testrun.gif"); //$NON-NLS-1$

	final Image fTestIgnoredIcon = TestRunnerViewPart
	.createImage("obj16/testignored.gif"); //$NON-NLS-1$

	final ImageDescriptor fSuiteIconDescriptor = GUnitPlugin
	.getImageDescriptor("obj16/tsuite.gif"); //$NON-NLS-1$

	final ImageDescriptor fSuiteOkIconDescriptor = GUnitPlugin
	.getImageDescriptor("obj16/tsuiteok.gif"); //$NON-NLS-1$

	final ImageDescriptor fSuiteErrorIconDescriptor = GUnitPlugin
	.getImageDescriptor("obj16/tsuiteerror.gif"); //$NON-NLS-1$

	final ImageDescriptor fSuiteFailIconDescriptor = GUnitPlugin
	.getImageDescriptor("obj16/tsuitefail.gif"); //$NON-NLS-1$

	final ImageDescriptor fSuiteRunningIconDescriptor = GUnitPlugin
	.getImageDescriptor("obj16/tsuiterun.gif"); //$NON-NLS-1$

	final Image fSuiteIcon = this.fSuiteIconDescriptor.createImage();

	final Image fSuiteOkIcon = this.fSuiteOkIconDescriptor.createImage();

	final Image fSuiteErrorIcon = this.fSuiteErrorIconDescriptor.createImage();

	final Image fSuiteFailIcon = this.fSuiteFailIconDescriptor.createImage();

	final Image fSuiteRunningIcon = this.fSuiteRunningIconDescriptor
	.createImage();

	// Persistence tags.
	static final String TAG_PAGE = "page"; //$NON-NLS-1$

	static final String TAG_RATIO = "ratio"; //$NON-NLS-1$

	static final String TAG_TRACEFILTER = "tracefilter"; //$NON-NLS-1$

	static final String TAG_ORIENTATION = "orientation"; //$NON-NLS-1$

	static final String TAG_SCROLL = "scroll"; //$NON-NLS-1$

	/**
	 * @since 3.2
	 */
	static final String TAG_LAYOUT = "layout"; //$NON-NLS-1$

	/**
	 * @since 3.2
	 */
	static final String TAG_FAILURES_ONLY = "failuresOnly"; //$NON-NLS-1$

	// orientations
	static final int VIEW_ORIENTATION_VERTICAL = 0;

	static final int VIEW_ORIENTATION_HORIZONTAL = 1;

	static final int VIEW_ORIENTATION_AUTOMATIC = 2;

	private IMemento fMemento;

	Image fOriginalViewImage;

	IElementChangedListener fDirtyListener;

	// private CTabFolder fTabFolder;
	private SashForm fSashForm;

	private Composite fCounterComposite;

	private Composite fParent;

	/**
	 * A Job that periodically updates view description, counters, and progress
	 * bar.
	 */
	private UpdateUIJob fUpdateJob;

	/**
	 * A Job that runs as long as a test run is running. It is used to show
	 * busyness for running jobs in the view (title in italics).
	 */
	private JUnitIsRunningJob fJUnitIsRunningJob;

	private ILock fJUnitIsRunningLock;

	public static final Object FAMILY_JUNIT_RUN = new Object();

	private final IPartListener2 fPartListener = new IPartListener2() {
		public void partActivated(final IWorkbenchPartReference ref) {
		}

		public void partBroughtToTop(final IWorkbenchPartReference ref) {
		}

		public void partInputChanged(final IWorkbenchPartReference ref) {
		}

		public void partClosed(final IWorkbenchPartReference ref) {
		}

		public void partDeactivated(final IWorkbenchPartReference ref) {
		}

		public void partOpened(final IWorkbenchPartReference ref) {
		}

		public void partVisible(final IWorkbenchPartReference ref) {
			if (getSite().getId().equals(ref.getId())) {
				TestRunnerViewPart.this.fPartIsVisible = true;
			}
		}

		public void partHidden(final IWorkbenchPartReference ref) {
			if (getSite().getId().equals(ref.getId())) {
				TestRunnerViewPart.this.fPartIsVisible = false;
			}
		}
	};

	protected boolean fPartIsVisible = false;

	private class RunnerViewHistory /* extends ViewHistory */{

		public void configureHistoryListAction(final IAction action) {
			action.setText(GUnitMessages.TestRunnerViewPart_history);
		}

		public void configureHistoryDropDownAction(final IAction action) {
			action
			.setToolTipText(GUnitMessages.TestRunnerViewPart_test_run_history);
			GUnitPlugin.setLocalImageDescriptors(action, "history_list.gif"); //$NON-NLS-1$
		}

		public Action getClearAction() {
			return new ClearAction();
		}

		public String getHistoryListDialogTitle() {
			return GUnitMessages.TestRunnerViewPart_test_runs;
		}

		public String getHistoryListDialogMessage() {
			return GUnitMessages.TestRunnerViewPart_select_test_run;
		}

		public Shell getShell() {
			return TestRunnerViewPart.this.fParent.getShell();
		}

		public List<TestRunSession> getHistoryEntries() {
			return GUnitPlugin.getModel().getTestRunSessions();
		}

		public Object getCurrentEntry() {
			return TestRunnerViewPart.this.fTestRunSession;
		}

		public void setActiveEntry(final Object entry) {
			final TestRunSession deactivatedSession = setActiveTestRunSession((TestRunSession) entry);
			if (deactivatedSession != null) {
				deactivatedSession.swapOut();
			}
		}

		public void setHistoryEntries(final List<TestRunSession> remainingEntries,
				final Object activeEntry) {
			setActiveTestRunSession((TestRunSession) activeEntry);

			final List<TestRunSession> testRunSessions = GUnitPlugin.getModel()
			.getTestRunSessions();
			testRunSessions.removeAll(remainingEntries);
			for (final Iterator<TestRunSession> iter = testRunSessions.iterator(); iter
			.hasNext();) {
				GUnitPlugin.getModel().removeTestRunSession(iter.next());
			}
			for (final Iterator<TestRunSession> iter = remainingEntries.iterator(); iter
			.hasNext();) {
				final TestRunSession remaining = iter.next();
				remaining.swapOut();
			}
		}

		public ImageDescriptor getImageDescriptor(final Object element) {
			final TestRunSession session = (TestRunSession) element;
			if (session.isStopped()) {
				return TestRunnerViewPart.this.fSuiteIconDescriptor;
			}

			if (session.isRunning()) {
				return TestRunnerViewPart.this.fSuiteRunningIconDescriptor;
			}

			final Result result = session.getTestResult(true);
			if (result == Result.OK) {
				return TestRunnerViewPart.this.fSuiteOkIconDescriptor;
			} else if (result == Result.ERROR) {
				return TestRunnerViewPart.this.fSuiteErrorIconDescriptor;
			} else if (result == Result.FAILURE) {
				return TestRunnerViewPart.this.fSuiteFailIconDescriptor;
			} else {
				return TestRunnerViewPart.this.fSuiteIconDescriptor;
			}
		}

		public String getText(final Object element) {
			final TestRunSession session = (TestRunSession) element;
			if (session.getStartTime() == 0) {
				return session.getTestRunName();
			} else {
				final String startTime = DateFormat.getDateTimeInstance().format(
						new Date(session.getStartTime()));
				return Messages.format(
						GUnitMessages.TestRunnerViewPart_testName_startTime,
						new Object[] { session.getTestRunName(), startTime });
			}
		}

		public void addMenuEntries(final MenuManager manager) {
			manager.appendToGroup(IWorkbenchActionConstants.MB_ADDITIONS,
					new ImportTestRunSessionAction(
							TestRunnerViewPart.this.fParent.getShell()));
			if (TestRunnerViewPart.this.fTestRunSession != null) {
				manager.appendToGroup(IWorkbenchActionConstants.MB_ADDITIONS,
						new ExportTestRunSessionAction(
								TestRunnerViewPart.this.fParent.getShell(),
								TestRunnerViewPart.this.fTestRunSession));
			}
		}

		public String getMaxEntriesMessage() {
			return GUnitMessages.TestRunnerViewPart_max_remembered;
		}

		public int getMaxEntries() {
			final IPreferenceStore store = GUnitPlugin.getDefault()
			.getPreferenceStore();
			return store.getInt(GUnitPreferencesConstants.MAX_TEST_RUNS);
		}

		public void setMaxEntries(final int maxEntries) {
			final IPreferenceStore store = GUnitPlugin.getDefault()
			.getPreferenceStore();
			store.setValue(GUnitPreferencesConstants.MAX_TEST_RUNS, maxEntries);
		}
	}

	private static class ImportTestRunSessionAction extends Action {
		private final Shell fShell;

		public ImportTestRunSessionAction(final Shell shell) {
			super(
					GUnitMessages.TestRunnerViewPart_ImportTestRunSessionAction_name);
			this.fShell = shell;
		}

		@Override
		public void run() {
			final FileDialog importDialog = new FileDialog(this.fShell, SWT.OPEN);
			importDialog
			.setText(GUnitMessages.TestRunnerViewPart_ImportTestRunSessionAction_title);
			importDialog.setFilterExtensions(new String[] { "*.xml", "*.*" }); //$NON-NLS-1$ //$NON-NLS-2$
			final String path = importDialog.open();
			if (path == null) {
				return;
			}

			// TODO: MULTI: getFileNames()
			final File file = new File(path);

			try {
				GUnitModel.importTestRunSession(file);
			} catch (final CoreException e) {
				GUnitPlugin.log(e);
				ErrorDialog
				.openError(
						this.fShell,
						GUnitMessages.TestRunnerViewPart_ImportTestRunSessionAction_error_title,
						e.getStatus().getMessage(), e.getStatus());
			}
		}
	}

	private static class ExportTestRunSessionAction extends Action {
		private final TestRunSession fTestRunSession;

		private final Shell fShell;

		public ExportTestRunSessionAction(final Shell shell,
				final TestRunSession testRunSession) {
			super(
					GUnitMessages.TestRunnerViewPart_ExportTestRunSessionAction_name);
			this.fShell = shell;
			this.fTestRunSession = testRunSession;
		}

		@Override
		public void run() {
			final FileDialog exportDialog = new FileDialog(this.fShell, SWT.SAVE);
			exportDialog
			.setText(GUnitMessages.TestRunnerViewPart_ExportTestRunSessionAction_title);
			exportDialog.setFileName(getFileName());
			exportDialog.setFilterExtensions(new String[] { "*.xml", "*.*" }); //$NON-NLS-1$ //$NON-NLS-2$
			final String path = exportDialog.open();
			if (path == null) {
				return;
			}

			// TODO: MULTI: getFileNames()
			final File file = new File(path);

			try {
				GUnitModel.exportTestRunSession(this.fTestRunSession, file);
			} catch (final CoreException e) {
				GUnitPlugin.log(e);
				ErrorDialog
				.openError(
						this.fShell,
						GUnitMessages.TestRunnerViewPart_ExportTestRunSessionAction_error_title,
						e.getStatus().getMessage(), e.getStatus());
			}
		}

		private String getFileName() {
			final String testRunName = this.fTestRunSession.getTestRunName();
			final long startTime = this.fTestRunSession.getStartTime();
			if (startTime == 0) {
				return testRunName;
			}

			final String isoTime = new SimpleDateFormat("yyyyMMdd-HHmmss").format(new Date(startTime)); //$NON-NLS-1$
			return testRunName + " " + isoTime + ".xml"; //$NON-NLS-1$ //$NON-NLS-2$
		}
	}

	private class TestRunSessionListener implements ITestRunSessionListener {
		public void sessionAdded(final TestRunSession testRunSession) {
			if (getSite().getWorkbenchWindow() == GUnitPlugin
					.getActiveWorkbenchWindow()) {
				final TestRunSession deactivatedSession = setActiveTestRunSession(testRunSession);
				if (deactivatedSession != null) {
					deactivatedSession.swapOut();
				}
				final String testRunName = TestRunnerViewPart.this.fTestRunSession
				.getTestRunName();
				String msg;
				if (testRunSession.getLaunch() != null) {
					msg = Messages.format(
							GUnitMessages.TestRunnerViewPart_Launching,
							new Object[] { testRunName });
				} else {
					msg = testRunName;
				}
				setContentDescription(msg);
			}
		}

		public void sessionRemoved(final TestRunSession testRunSession) {
			if (testRunSession.equals(TestRunnerViewPart.this.fTestRunSession)) {
				final List<TestRunSession> testRunSessions = GUnitPlugin.getModel()
				.getTestRunSessions();
				TestRunSession deactivatedSession;
				if (!testRunSessions.isEmpty()) {
					deactivatedSession = setActiveTestRunSession(testRunSessions
							.get(0));
				} else {
					deactivatedSession = setActiveTestRunSession(null);
				}
				if (deactivatedSession != null) {
					deactivatedSession.swapOut();
				}
			}
		}
	}

	private class TestSessionListener implements ITestSessionListener {
		public void sessionStarted() {
			TestRunnerViewPart.this.fTestViewer.registerViewersRefresh();
			TestRunnerViewPart.this.fShowOnErrorOnly = getShowOnErrorOnly();

			startUpdateJobs();

			TestRunnerViewPart.this.fStopAction.setEnabled(true);
			TestRunnerViewPart.this.fRerunLastTestAction.setEnabled(true);
		}

		public void sessionEnded(final long elapsedTime) {
			TestRunnerViewPart.this.fTestViewer.registerAutoScrollTarget(null);

			final String[] keys = { elapsedTimeAsString(elapsedTime) };
			final String msg = Messages.format(
					GUnitMessages.TestRunnerViewPart_message_finish, keys);
			registerInfoMessage(msg);

			postSyncRunnable(new Runnable() {
				public void run() {
					if (isDisposed()) {
						return;
					}
					TestRunnerViewPart.this.fStopAction
					.setEnabled(lastLaunchIsKeptAlive());
					updateRerunFailedFirstAction();
					processChangesInUI();
					if (hasErrorsOrFailures()) {
						selectFirstFailure();
					}
					if (TestRunnerViewPart.this.fDirtyListener == null) {
						// fDirtyListener = new DirtyListener();
						// JavaCore.addElementChangedListener(fDirtyListener);
					}
					warnOfContentChange();
				}
			});
			stopUpdateJobs();
		}

		public void sessionStopped(final long elapsedTime) {
			TestRunnerViewPart.this.fTestViewer.registerAutoScrollTarget(null);

			registerInfoMessage(GUnitMessages.TestRunnerViewPart_message_stopped);
			handleStopped();
		}

		public void sessionTerminated() {
			TestRunnerViewPart.this.fTestViewer.registerAutoScrollTarget(null);

			registerInfoMessage(GUnitMessages.TestRunnerViewPart_message_terminated);
			handleStopped();
		}

		public void runningBegins() {
			if (!TestRunnerViewPart.this.fShowOnErrorOnly) {
				postShowTestResultsView();
			}
		}

		public void testStarted(final TestCaseElement testCaseElement) {
			TestRunnerViewPart.this.fTestViewer
			.registerAutoScrollTarget(testCaseElement);
			TestRunnerViewPart.this.fTestViewer
			.registerViewerUpdate(testCaseElement);

			final String className = testCaseElement.getClassName();
			final String method = testCaseElement.getTestMethodName();
			final String status = Messages.format(
					GUnitMessages.TestRunnerViewPart_message_started,
					new String[] { className, method });
			registerInfoMessage(status);
		}

		public void testFailed(final TestElement testElement,
				final TestElement.Status status, final String trace, final String expected,
				final String actual) {
			if (isAutoScroll()) {
				TestRunnerViewPart.this.fTestViewer
				.registerFailedForAutoScroll(testElement);
			}
			TestRunnerViewPart.this.fTestViewer
			.registerViewerUpdate(testElement);

			// show the view on the first error only
			if (TestRunnerViewPart.this.fShowOnErrorOnly
					&& (getErrorsPlusFailures() == 1)) {
				postShowTestResultsView();
			}

			// TODO:
			// [Bug 35590] JUnit window doesn't report errors from
			// junit.extensions.TestSetup [JUnit]
			// when a failure occurs in test setup then no test is running
			// to update the views we artificially signal the end of a test run
			// if (!fTestIsRunning) {
			// fTestIsRunning= false;
			// testEnded(testCaseElement);
			// }
		}

		public void testEnded(final TestCaseElement testCaseElement) {
			TestRunnerViewPart.this.fTestViewer
			.registerViewerUpdate(testCaseElement);
		}

		public void testReran(final TestCaseElement testCaseElement,
				final TestElement.Status status, final String trace, final String expectedResult,
				final String actualResult) {
			TestRunnerViewPart.this.fTestViewer
			.registerViewerUpdate(testCaseElement); // TODO:
			// autoExpand?
			postSyncProcessChanges();
			showFailure(testCaseElement);
		}

		public void testAdded(final TestElement testElement) {
			TestRunnerViewPart.this.fTestViewer.registerTestAdded(testElement);
		}

		public boolean acceptsSwapToDisk() {
			return false;
		}
	}

	private class UpdateUIJob extends UIJob {
		private boolean fRunning = true;

		public UpdateUIJob(final String name) {
			super(name);
			setSystem(true);
		}

		@Override
		public IStatus runInUIThread(final IProgressMonitor monitor) {
			if (!isDisposed()) {
				processChangesInUI();
			}
			schedule(REFRESH_INTERVAL);
			return Status.OK_STATUS;
		}

		public void stop() {
			this.fRunning = false;
		}

		@Override
		public boolean shouldSchedule() {
			return this.fRunning;
		}
	}

	private class JUnitIsRunningJob extends Job {
		public JUnitIsRunningJob(final String name) {
			super(name);
			setSystem(true);
		}

		@Override
		public IStatus run(final IProgressMonitor monitor) {
			// wait until the test run terminates
			TestRunnerViewPart.this.fJUnitIsRunningLock.acquire();
			return Status.OK_STATUS;
		}

		@Override
		public boolean belongsTo(final Object family) {
			return family == TestRunnerViewPart.FAMILY_JUNIT_RUN;
		}
	}

	private class ClearAction extends Action {
		public ClearAction() {
			setText(GUnitMessages.TestRunnerViewPart_clear_history_label);

			boolean enabled = false;
			final List<TestRunSession> testRunSessions = GUnitPlugin.getModel()
			.getTestRunSessions();
			for (final Iterator<TestRunSession> iter = testRunSessions.iterator(); iter
			.hasNext();) {
				final TestRunSession testRunSession = iter.next();
				if (!testRunSession.isRunning()) {
					enabled = true;
					break;
				}
			}
			setEnabled(enabled);
		}

		@Override
		public void run() {
			final List<TestRunSession> testRunSessions = getRunningSessions();
			final Object first = testRunSessions.isEmpty() ? null : testRunSessions
					.get(0);
			TestRunnerViewPart.this.fViewHistory.setHistoryEntries(
					testRunSessions, first);
		}

		private List<TestRunSession> getRunningSessions() {
			final List<TestRunSession> testRunSessions = GUnitPlugin.getModel()
			.getTestRunSessions();
			for (final Iterator<TestRunSession> iter = testRunSessions.iterator(); iter
			.hasNext();) {
				final TestRunSession testRunSession = iter.next();
				if (!testRunSession.isRunning()) {
					iter.remove();
				}
			}
			return testRunSessions;
		}
	}

	private class StopAction extends Action {
		public StopAction() {
			setText(GUnitMessages.TestRunnerViewPart_stopaction_text);
			setToolTipText(GUnitMessages.TestRunnerViewPart_stopaction_tooltip);
			GUnitPlugin.setLocalImageDescriptors(this, "stop.gif"); //$NON-NLS-1$
		}

		@Override
		public void run() {
			stopTest();
			setEnabled(false);
		}
	}

	private class RerunLastAction extends Action {
		public RerunLastAction() {
			setText(GUnitMessages.TestRunnerViewPart_rerunaction_label);
			setToolTipText(GUnitMessages.TestRunnerViewPart_rerunaction_tooltip);
			GUnitPlugin.setLocalImageDescriptors(this, "relaunch.gif"); //$NON-NLS-1$
			setEnabled(false);
			setActionDefinitionId(RERUN_LAST_COMMAND);
		}

		@Override
		public void run() {
			rerunTestRun();
		}
	}

	private class RerunLastFailedFirstAction extends Action {
		public RerunLastFailedFirstAction() {
			setText(GUnitMessages.TestRunnerViewPart_rerunfailuresaction_label);
			setToolTipText(GUnitMessages.TestRunnerViewPart_rerunfailuresaction_tooltip);
			GUnitPlugin.setLocalImageDescriptors(this, "relaunchf.gif"); //$NON-NLS-1$
			setEnabled(false);
			setActionDefinitionId(RERUN_FAILED_FIRST_COMMAND);
		}

		@Override
		public void run() {
			rerunTestFailedFirst();
		}
	}

	private class ToggleOrientationAction extends Action {
		private final int fActionOrientation;

		public ToggleOrientationAction(final TestRunnerViewPart v, final int orientation) {
			super("", AS_RADIO_BUTTON); //$NON-NLS-1$
			if (orientation == TestRunnerViewPart.VIEW_ORIENTATION_HORIZONTAL) {
				setText(GUnitMessages.TestRunnerViewPart_toggle_horizontal_label);
				setImageDescriptor(GUnitPlugin
						.getImageDescriptor("elcl16/th_horizontal.gif")); //$NON-NLS-1$
			} else if (orientation == TestRunnerViewPart.VIEW_ORIENTATION_VERTICAL) {
				setText(GUnitMessages.TestRunnerViewPart_toggle_vertical_label);
				setImageDescriptor(GUnitPlugin
						.getImageDescriptor("elcl16/th_vertical.gif")); //$NON-NLS-1$
			} else if (orientation == TestRunnerViewPart.VIEW_ORIENTATION_AUTOMATIC) {
				setText(GUnitMessages.TestRunnerViewPart_toggle_automatic_label);
				setImageDescriptor(GUnitPlugin
						.getImageDescriptor("elcl16/th_automatic.gif")); //$NON-NLS-1$
			}
			this.fActionOrientation = orientation;
			PlatformUI
			.getWorkbench()
			.getHelpSystem()
			.setHelp(
					this,
					IGUnitHelpContextIds.RESULTS_VIEW_TOGGLE_ORIENTATION_ACTION);
		}

		public int getOrientation() {
			return this.fActionOrientation;
		}

		@Override
		public void run() {
			if (isChecked()) {
				TestRunnerViewPart.this.fOrientation = this.fActionOrientation;
				computeOrientation();
			}
		}
	}

	// /**
	// * Listen for for modifications to Java elements
	// */
	// private class DirtyListener implements IElementChangedListener {
	// public void elementChanged(ElementChangedEvent event) {
	// processDelta(event.getDelta());
	// }
	//
	// private boolean processDelta(IErlElementDelta delta) {
	// int kind = delta.getKind();
	// int details = delta.getFlags();
	// int type = delta.getElement().getElementType();
	//
	// switch (type) {
	// // Consider containers for class files.
	// case IErlElement.JAVA_MODEL:
	// case IErlElement.JAVA_PROJECT:
	// case IErlElement.PACKAGE_FRAGMENT_ROOT:
	// case IErlElement.PACKAGE_FRAGMENT:
	// // If we did something different than changing a child we flush
	// // the undo / redo stack.
	// if (kind != IErlElementDelta.CHANGED
	// || details != IErlElementDelta.F_CHILDREN) {
	// codeHasChanged();
	// return false;
	// }
	// break;
	// case IErlElement.COMPILATION_UNIT:
	// // if we have changed a primary working copy (e.g created,
	// // removed, ...)
	// // then we do nothing.
	// if ((details & IErlElementDelta.F_PRIMARY_WORKING_COPY) != 0) {
	// return true;
	// }
	// codeHasChanged();
	// return false;
	//
	// case IErlElement.CLASS_FILE:
	// // Don't examine children of a class file but keep on examining
	// // siblings.
	// return true;
	// default:
	// codeHasChanged();
	// return false;
	// }
	//
	// IErlElementDelta[] affectedChildren = delta.getAffectedChildren();
	// if (affectedChildren == null) {
	// return true;
	// }
	//
	// for (int i = 0; i < affectedChildren.length; i++) {
	// if (!processDelta(affectedChildren[i])) {
	// return false;
	// }
	// }
	// return true;
	// }
	// }

	private class FailuresOnlyFilterAction extends Action {
		public FailuresOnlyFilterAction() {
			super(GUnitMessages.TestRunnerViewPart_show_failures_only,
					AS_CHECK_BOX);
			setToolTipText(GUnitMessages.TestRunnerViewPart_show_failures_only);
			setImageDescriptor(GUnitPlugin
					.getImageDescriptor("obj16/failures.gif")); //$NON-NLS-1$
		}

		@Override
		public void run() {
			setShowFailuresOnly(isChecked());
		}
	}

	private class ShowTestHierarchyAction extends Action {

		public ShowTestHierarchyAction() {
			super(GUnitMessages.TestRunnerViewPart_hierarchical_layout,
					IAction.AS_CHECK_BOX);
			setImageDescriptor(GUnitPlugin
					.getImageDescriptor("elcl16/hierarchicalLayout.gif")); //$NON-NLS-1$
		}

		@Override
		public void run() {
			final int mode = isChecked() ? LAYOUT_HIERARCHICAL : LAYOUT_FLAT;
			setLayoutMode(mode);
		}
	}

	private class ActivateOnErrorAction extends Action {
		public ActivateOnErrorAction() {
			super(GUnitMessages.TestRunnerViewPart_activate_on_failure_only,
					IAction.AS_CHECK_BOX);
			//setImageDescriptor(GUnitPlugin.getImageDescriptor("obj16/failures.gif")); //$NON-NLS-1$
			update();
		}

		public void update() {
			setChecked(getShowOnErrorOnly());
		}

		@Override
		public void run() {
			final boolean checked = isChecked();
			TestRunnerViewPart.this.fShowOnErrorOnly = checked;
			final IPreferenceStore store = GUnitPlugin.getDefault()
			.getPreferenceStore();
			store.setValue(GUnitPreferencesConstants.SHOW_ON_ERROR_ONLY,
					checked);
		}
	}

	@Override
	public void init(final IViewSite site, final IMemento memento) throws PartInitException {
		super.init(site, memento);
		this.fMemento = memento;
		final IWorkbenchSiteProgressService progressService = getProgressService();
		if (progressService != null) {
			progressService
			.showBusyForFamily(TestRunnerViewPart.FAMILY_JUNIT_RUN);
		}
	}

	private IWorkbenchSiteProgressService getProgressService() {
		final Object siteService = getSite().getAdapter(
				IWorkbenchSiteProgressService.class);
		if (siteService != null) {
			return (IWorkbenchSiteProgressService) siteService;
		}
		return null;
	}

	@Override
	public void saveState(final IMemento memento) {
		if (this.fSashForm == null) {
			// part has not been created
			if (this.fMemento != null) {
				memento.putMemento(this.fMemento);
			}
			return;
		}

		// int activePage= fTabFolder.getSelectionIndex();
		// memento.putInteger(TAG_PAGE, activePage);
		memento.putString(TAG_SCROLL,
				this.fScrollLockAction.isChecked() ? "true" : "false"); //$NON-NLS-1$ //$NON-NLS-2$
		final int weigths[] = this.fSashForm.getWeights();
		final int ratio = (weigths[0] * 1000) / (weigths[0] + weigths[1]);
		memento.putInteger(TAG_RATIO, ratio);
		memento.putInteger(TAG_ORIENTATION, this.fOrientation);

		memento.putString(TAG_FAILURES_ONLY, this.fFailuresOnlyFilterAction
				.isChecked() ? "true" : "false"); //$NON-NLS-1$ //$NON-NLS-2$
		memento.putInteger(TAG_LAYOUT, this.fLayout);
	}

	private void restoreLayoutState(final IMemento memento) {
		// Integer page= memento.getInteger(TAG_PAGE);
		// if (page != null) {
		// int p= page.intValue();
		// if (p < fTestRunTabs.size()) { // tab count can decrease if a
		// contributing plug-in is removed
		// fTabFolder.setSelection(p);
		// fActiveRunTab= (TestRunTab)fTestRunTabs.get(p);
		// }
		// }
		final Integer ratio = memento.getInteger(TAG_RATIO);
		if (ratio != null) {
			this.fSashForm.setWeights(new int[] { ratio.intValue(),
					1000 - ratio.intValue() });
		}
		final Integer orientation = memento.getInteger(TAG_ORIENTATION);
		if (orientation != null) {
			this.fOrientation = orientation.intValue();
		}
		computeOrientation();
		final String scrollLock = memento.getString(TAG_SCROLL);
		if (scrollLock != null) {
			this.fScrollLockAction.setChecked(scrollLock.equals("true")); //$NON-NLS-1$
			setAutoScroll(!this.fScrollLockAction.isChecked());
		}

		final Integer layout = memento.getInteger(TAG_LAYOUT);
		int layoutValue = LAYOUT_HIERARCHICAL;
		if (layout != null) {
			layoutValue = layout.intValue();
		}

		final String failuresOnly = memento.getString(TAG_FAILURES_ONLY);
		boolean showFailuresOnly = false;
		if (failuresOnly != null) {
			showFailuresOnly = failuresOnly.equals("true"); //$NON-NLS-1$
		}

		setFilterAndLayout(showFailuresOnly, layoutValue);
	}

	/**
	 * Stops the currently running test and shuts down the RemoteTestRunner
	 */
	public void stopTest() {
		if (this.fTestRunSession != null) {
			if (this.fTestRunSession.isRunning()) {
				setContentDescription(GUnitMessages.TestRunnerViewPart_message_stopping);
			}
			this.fTestRunSession.stopTestRun();
		}
	}

	private void startUpdateJobs() {
		postSyncProcessChanges();

		if (this.fUpdateJob != null) {
			return;
		}
		this.fJUnitIsRunningJob = new JUnitIsRunningJob(
				GUnitMessages.TestRunnerViewPart_wrapperJobName);
		this.fJUnitIsRunningLock = Job.getJobManager().newLock();
		// acquire lock while a test run is running
		// the lock is released when the test run terminates
		// the wrapper job will wait on this lock.
		this.fJUnitIsRunningLock.acquire();
		getProgressService().schedule(this.fJUnitIsRunningJob);

		this.fUpdateJob = new UpdateUIJob(
				GUnitMessages.TestRunnerViewPart_jobName);
		this.fUpdateJob.schedule(REFRESH_INTERVAL);
	}

	private void stopUpdateJobs() {
		if (this.fUpdateJob != null) {
			this.fUpdateJob.stop();
			this.fUpdateJob = null;
		}
		if (this.fJUnitIsRunningJob != null && this.fJUnitIsRunningLock != null) {
			this.fJUnitIsRunningLock.release();
			this.fJUnitIsRunningJob = null;
		}
		postSyncProcessChanges();
	}

	private void processChangesInUI() {
		if (this.fSashForm.isDisposed()) {
			return;
		}

		doShowInfoMessage();
		refreshCounters();

		if (!this.fPartIsVisible) {
			updateViewTitleProgress();
		} else {
			updateViewIcon();
		}
		final boolean hasErrorsOrFailures = hasErrorsOrFailures();
		this.fNextAction.setEnabled(hasErrorsOrFailures);
		this.fPreviousAction.setEnabled(hasErrorsOrFailures);

		this.fTestViewer.processChangesInUI();
	}

	/**
	 * Stops the currently running test and shuts down the RemoteTestRunner
	 */
	public void rerunTestRun() {
		if (lastLaunchIsKeptAlive()) {
			// prompt for terminating the existing run
			if (MessageDialog.openQuestion(getSite().getShell(),
					GUnitMessages.TestRunnerViewPart_terminate_title,
					GUnitMessages.TestRunnerViewPart_terminate_message)) {
				stopTest(); // TODO: wait for termination
			}
		}

		if (this.fTestRunSession == null) {
			return;
		}
		final ILaunch launch = this.fTestRunSession.getLaunch();
		if (launch == null) {
			return;
		}
		final ILaunchConfiguration launchConfiguration = launch
		.getLaunchConfiguration();
		if (launchConfiguration == null) {
			return;
		}

		final ILaunchConfiguration configuration = prepareLaunchConfigForRelaunch(launchConfiguration);
		DebugUITools.launch(configuration, launch.getLaunchMode());
	}

	private ILaunchConfiguration prepareLaunchConfigForRelaunch(
			final ILaunchConfiguration configuration) {
		try {
			final String attribute = configuration.getAttribute(
					GUnitLaunchConfigurationConstants.ATTR_FAILURES_NAMES, ""); //$NON-NLS-1$
			if (attribute.length() != 0) {
				final String configName = Messages.format(
						GUnitMessages.TestRunnerViewPart_configName,
						configuration.getName());
				final ILaunchConfigurationWorkingCopy tmp = configuration
				.copy(configName);
				tmp.setAttribute(
						GUnitLaunchConfigurationConstants.ATTR_FAILURES_NAMES,
				""); //$NON-NLS-1$
				return tmp;
			}
		} catch (final CoreException e) {
			// fall through
		}
		return configuration;
	}

	public void rerunTestFailedFirst() {
		if (lastLaunchIsKeptAlive()) {
			// prompt for terminating the existing run
			if (MessageDialog.openQuestion(getSite().getShell(),
					GUnitMessages.TestRunnerViewPart_terminate_title,
					GUnitMessages.TestRunnerViewPart_terminate_message)) {
				if (this.fTestRunSession != null) {
					this.fTestRunSession.stopTestRun();
				}
			}
		}
		final ILaunch launch = this.fTestRunSession.getLaunch();
		if (launch != null && launch.getLaunchConfiguration() != null) {
			final ILaunchConfiguration launchConfiguration = launch
			.getLaunchConfiguration();
			if (launchConfiguration != null) {
				try {
					final String oldName = launchConfiguration.getName();
					final String oldFailuresFilename = launchConfiguration
					.getAttribute(
							GUnitLaunchConfigurationConstants.ATTR_FAILURES_NAMES,
							(String) null);
					String configName;
					if (oldFailuresFilename != null) {
						configName = oldName;
					} else {
						configName = Messages
						.format(
								GUnitMessages.TestRunnerViewPart_rerunFailedFirstLaunchConfigName,
								oldName);
					}
					final ILaunchConfigurationWorkingCopy tmp = launchConfiguration
					.copy(configName);
					tmp
					.setAttribute(
							GUnitLaunchConfigurationConstants.ATTR_FAILURES_NAMES,
							createFailureNamesFile());
					tmp.launch(launch.getLaunchMode(), null);
					return;
				} catch (final CoreException e) {
					ErrorDialog.openError(getSite().getShell(),
							GUnitMessages.TestRunnerViewPart_error_cannotrerun,
							e.getMessage(), e.getStatus());
				}
			}
			MessageDialog.openInformation(getSite().getShell(),
					GUnitMessages.TestRunnerViewPart_cannotrerun_title,
					GUnitMessages.TestRunnerViewPart_cannotrerurn_message);
		}
	}

	private String createFailureNamesFile() throws CoreException {
		try {
			final File file = File.createTempFile("testFailures", ".txt"); //$NON-NLS-1$ //$NON-NLS-2$
			file.deleteOnExit();
			final TestElement[] failures = this.fTestRunSession
			.getAllFailedTestElements();
			BufferedWriter bw = null;
			try {
				bw = new BufferedWriter(new FileWriter(file));
				for (int i = 0; i < failures.length; i++) {
					final TestElement testElement = failures[i];
					bw.write(testElement.getTestName());
					bw.newLine();
				}
			} finally {
				if (bw != null) {
					bw.close();
				}
			}
			return file.getAbsolutePath();
		} catch (final IOException e) {
			throw new CoreException(new Status(IStatus.ERROR,
					GUnitPlugin.PLUGIN_ID, IStatus.ERROR, "", e)); //$NON-NLS-1$
		}
	}

	public void setAutoScroll(final boolean scroll) {
		this.fAutoScroll = scroll;
	}

	public boolean isAutoScroll() {
		return this.fAutoScroll;
	}

	public void selectNextFailure() {
		this.fTestViewer.selectFailure(true);
	}

	public void selectPreviousFailure() {
		this.fTestViewer.selectFailure(false);
	}

	protected void selectFirstFailure() {
		this.fTestViewer.selectFirstFailure();
	}

	private boolean hasErrorsOrFailures() {
		return getErrorsPlusFailures() > 0;
	}

	private int getErrorsPlusFailures() {
		if (this.fTestRunSession == null) {
			return 0;
		} else {
			return this.fTestRunSession.getErrorCount()
			+ this.fTestRunSession.getFailureCount();
		}
	}

	private String elapsedTimeAsString(final long runTime) {
		return NumberFormat.getInstance().format((double) runTime / 1000);
	}

	private void handleStopped() {
		postSyncRunnable(new Runnable() {
			public void run() {
				if (isDisposed()) {
					return;
				}
				resetViewIcon();
				TestRunnerViewPart.this.fStopAction.setEnabled(false);
				updateRerunFailedFirstAction();
			}
		});
		stopUpdateJobs();
	}

	private void resetViewIcon() {
		this.fViewImage = this.fOriginalViewImage;
		firePropertyChange(IWorkbenchPart.PROP_TITLE);
	}

	private void updateViewIcon() {
		if (this.fTestRunSession == null || this.fTestRunSession.isStopped()
				|| this.fTestRunSession.isRunning()
				|| this.fTestRunSession.getStartedCount() == 0) {
			this.fViewImage = this.fOriginalViewImage;
		} else if (hasErrorsOrFailures()) {
			this.fViewImage = this.fTestRunFailIcon;
		} else {
			this.fViewImage = this.fTestRunOKIcon;
		}
		firePropertyChange(IWorkbenchPart.PROP_TITLE);
	}

	private void updateViewTitleProgress() {
		if (this.fTestRunSession != null) {
			if (this.fTestRunSession.isRunning()) {
				final Image progress = this.fProgressImages.getImage(
						this.fTestRunSession.getStartedCount(),
						this.fTestRunSession.getTotalCount(),
						this.fTestRunSession.getErrorCount(),
						this.fTestRunSession.getFailureCount());
				if (progress != this.fViewImage) {
					this.fViewImage = progress;
					firePropertyChange(IWorkbenchPart.PROP_TITLE);
				}
			} else {
				updateViewIcon();
			}
		} else {
			resetViewIcon();
		}
	}

	/**
	 * @param testRunSession
	 *            new active test run session
	 * @return deactivated session, or <code>null</code> iff no session got
	 *         deactivated
	 */
	private TestRunSession setActiveTestRunSession(final TestRunSession testRunSession) {
		/*
		 * - State: fTestRunSession fTestSessionListener Jobs
		 * fTestViewer.processChangesInUI(); - UI: fCounterPanel fProgressBar
		 * setContentDescription / fInfoMessage setTitleToolTip view icons
		 * statusLine fFailureTrace
		 * 
		 * action enablement
		 */
		if (this.fTestRunSession == testRunSession) {
			return null;
		}

		if (this.fTestRunSession != null && this.fTestSessionListener != null) {
			this.fTestRunSession
			.removeTestSessionListener(this.fTestSessionListener);
			this.fTestSessionListener = null;
		}

		final TestRunSession deactivatedSession = this.fTestRunSession;

		this.fTestRunSession = testRunSession;
		this.fTestViewer.registerActiveSession(testRunSession);

		if (this.fSashForm.isDisposed()) {
			stopUpdateJobs();
			return deactivatedSession;
		}

		if (testRunSession == null) {
			setTitleToolTip(null);
			resetViewIcon();
			clearStatus();
			this.fFailureTrace.clear();

			registerInfoMessage(" "); //$NON-NLS-1$
			stopUpdateJobs();

			this.fStopAction.setEnabled(false);
			this.fRerunFailedFirstAction.setEnabled(false);
			this.fRerunLastTestAction.setEnabled(false);

		} else {
			this.fTestSessionListener = new TestSessionListener();
			this.fTestRunSession
			.addTestSessionListener(this.fTestSessionListener);

			setTitleToolTip();

			clearStatus();
			this.fFailureTrace.clear();
			registerInfoMessage(this.fTestRunSession.getTestRunName());

			updateRerunFailedFirstAction();
			this.fRerunLastTestAction.setEnabled(this.fTestRunSession
					.getLaunch() != null);

			if (this.fTestRunSession.isRunning()) {
				startUpdateJobs();

				this.fStopAction.setEnabled(true);

			} else /* old or fresh session: don't want jobs at this stage */{
				stopUpdateJobs();

				this.fStopAction.setEnabled(this.fTestRunSession.isKeptAlive());
				this.fTestViewer.expandFirstLevel();
			}
		}
		return deactivatedSession;
	}

	private void updateRerunFailedFirstAction() {
	}

	/**
	 * @return the display name of the current test run sessions kind, or
	 *         <code>null</code>
	 */
	public String getTestKindDisplayName() {
		final ITestKind kind = this.fTestRunSession.getTestRunnerKind();
		if (!kind.isNull()) {
			return kind.getDisplayName();
		}
		return null;
	}

	private void setTitleToolTip() {
		final String testKindDisplayStr = getTestKindDisplayName();

		if (testKindDisplayStr != null) {
			setTitleToolTip(MessageFormat.format(
					GUnitMessages.TestRunnerViewPart_titleToolTip,
					this.fTestRunSession.getTestRunName(), testKindDisplayStr));
		} else {
			setTitleToolTip(this.fTestRunSession.getTestRunName());
		}
	}

	@Override
	public synchronized void dispose() {
		this.fIsDisposed = true;
		if (this.fTestRunSessionListener != null) {
			GUnitPlugin.getModel().removeTestRunSessionListener(
					this.fTestRunSessionListener);
		}

		final IHandlerService handlerService = (IHandlerService) getSite()
		.getWorkbenchWindow().getService(IHandlerService.class);
		handlerService.deactivateHandler(this.fRerunLastActivation);
		handlerService.deactivateHandler(this.fRerunFailedFirstActivation);
		setActiveTestRunSession(null);

		if (this.fProgressImages != null) {
			this.fProgressImages.dispose();
		}
		getViewSite().getPage().removePartListener(this.fPartListener);

		disposeImages();
		if (this.fClipboard != null) {
			this.fClipboard.dispose();
		}
		if (this.fViewMenuListener != null) {
			getViewSite().getActionBars().getMenuManager().removeMenuListener(
					this.fViewMenuListener);
		}
	}

	private void disposeImages() {
		this.fTestRunOKIcon.dispose();
		this.fTestRunFailIcon.dispose();
		this.fStackViewIcon.dispose();
		this.fTestRunOKDirtyIcon.dispose();
		this.fTestRunFailDirtyIcon.dispose();

		this.fTestIcon.dispose();
		this.fTestRunningIcon.dispose();
		this.fTestOkIcon.dispose();
		this.fTestErrorIcon.dispose();
		this.fTestFailIcon.dispose();
		this.fTestIgnoredIcon.dispose();

		this.fSuiteIcon.dispose();
		this.fSuiteOkIcon.dispose();
		this.fSuiteRunningIcon.dispose();
		this.fSuiteErrorIcon.dispose();
		this.fSuiteFailIcon.dispose();
	}

	private void postSyncRunnable(final Runnable r) {
		if (!isDisposed()) {
			getDisplay().syncExec(r);
		}
	}

	private void refreshCounters() {
		// TODO: Inefficient. Either
		// - keep a boolean fHasTestRun and update only on changes, or
		// - improve components to only redraw on changes (once!).

		int startedCount;
		int ignoredCount;
		int totalCount;
		int errorCount;
		int failureCount;
		boolean hasErrorsOrFailures;
		boolean stopped;

		if (this.fTestRunSession != null) {
			startedCount = this.fTestRunSession.getStartedCount();
			ignoredCount = this.fTestRunSession.getIgnoredCount();
			totalCount = this.fTestRunSession.getTotalCount();
			errorCount = this.fTestRunSession.getErrorCount();
			failureCount = this.fTestRunSession.getFailureCount();
			hasErrorsOrFailures = errorCount + failureCount > 0;
			stopped = this.fTestRunSession.isStopped();
		} else {
			startedCount = 0;
			ignoredCount = 0;
			totalCount = 0;
			errorCount = 0;
			failureCount = 0;
			hasErrorsOrFailures = false;
			stopped = false;
		}

		this.fCounterPanel.setTotal(totalCount);
		this.fCounterPanel.setRunValue(startedCount, ignoredCount);
		this.fCounterPanel.setErrorValue(errorCount);
		this.fCounterPanel.setFailureValue(failureCount);

		int ticksDone;
		if (startedCount == 0) {
			ticksDone = 0;
		} else if (startedCount == totalCount
				&& !this.fTestRunSession.isRunning()) {
			ticksDone = totalCount;
		} else {
			ticksDone = startedCount - 1;
		}

		this.fProgressBar.reset(hasErrorsOrFailures, stopped, ticksDone,
				totalCount);
	}

	protected void postShowTestResultsView() {
		postSyncRunnable(new Runnable() {
			public void run() {
				if (isDisposed()) {
					return;
				}
				showTestResultsView();
			}
		});
	}

	public void showTestResultsView() {
		final IWorkbenchWindow window = getSite().getWorkbenchWindow();
		final IWorkbenchPage page = window.getActivePage();
		TestRunnerViewPart testRunner = null;

		if (page != null) {
			try { // show the result view
				testRunner = (TestRunnerViewPart) page
				.findView(TestRunnerViewPart.NAME);
				if (testRunner == null) {
					final IWorkbenchPart activePart = page.getActivePart();
					testRunner = (TestRunnerViewPart) page
					.showView(TestRunnerViewPart.NAME);
					// restore focus
					page.activate(activePart);
				} else {
					page.bringToTop(testRunner);
				}
			} catch (final PartInitException pie) {
				GUnitPlugin.log(pie);
			}
		}
	}

	protected void doShowInfoMessage() {
		if (this.fInfoMessage != null) {
			setContentDescription(this.fInfoMessage);
			this.fInfoMessage = null;
		}
	}

	protected void registerInfoMessage(final String message) {
		this.fInfoMessage = message;
	}

	private SashForm createSashForm(final Composite parent) {
		this.fSashForm = new SashForm(parent, SWT.VERTICAL);

		final ViewForm top = new ViewForm(this.fSashForm, SWT.NONE);

		final Composite empty = new Composite(top, SWT.NONE);
		empty.setLayout(new Layout() {
			@Override
			protected Point computeSize(final Composite composite, final int wHint,
					final int hHint, final boolean flushCache) {
				return new Point(1, 1); // (0, 0) does not work with
				// super-intelligent ViewForm
			}

			@Override
			protected void layout(final Composite composite, final boolean flushCache) {
			}
		});
		top.setTopLeft(empty); // makes ViewForm draw the horizontal separator
		// line ...
		this.fTestViewer = new TestViewer(top, this.fClipboard, this);
		top.setContent(this.fTestViewer.getTestViewerControl());

		final ViewForm bottom = new ViewForm(this.fSashForm, SWT.NONE);

		final CLabel label = new CLabel(bottom, SWT.NONE);
		label.setText(GUnitMessages.TestRunnerViewPart_label_failure);
		label.setImage(this.fStackViewIcon);
		bottom.setTopLeft(label);
		final ToolBar failureToolBar = new ToolBar(bottom, SWT.FLAT | SWT.WRAP);
		bottom.setTopCenter(failureToolBar);
		this.fFailureTrace = new FailureTrace(bottom, this.fClipboard, this,
				failureToolBar);
		bottom.setContent(this.fFailureTrace.getComposite());

		this.fSashForm.setWeights(new int[] { 50, 50 });
		return this.fSashForm;
	}

	private void clearStatus() {
		getStatusLine().setMessage(null);
		getStatusLine().setErrorMessage(null);
	}

	@Override
	public void setFocus() {
		if (this.fTestViewer != null) {
			this.fTestViewer.getTestViewerControl().setFocus();
		}
	}

	@Override
	public void createPartControl(final Composite parent) {
		this.fParent = parent;
		addResizeListener(parent);
		this.fClipboard = new Clipboard(parent.getDisplay());

		final GridLayout gridLayout = new GridLayout();
		gridLayout.marginWidth = 0;
		gridLayout.marginHeight = 0;
		parent.setLayout(gridLayout);

		this.fViewHistory = new RunnerViewHistory();
		configureToolBar();

		this.fCounterComposite = createProgressCountPanel(parent);
		this.fCounterComposite.setLayoutData(new GridData(
				GridData.GRAB_HORIZONTAL | GridData.HORIZONTAL_ALIGN_FILL));
		final SashForm sashForm = createSashForm(parent);
		sashForm.setLayoutData(new GridData(GridData.FILL_BOTH));

		final IActionBars actionBars = getViewSite().getActionBars();
		this.fCopyAction = new GUnitCopyAction(this.fFailureTrace,
				this.fClipboard);
		actionBars.setGlobalActionHandler(ActionFactory.COPY.getId(),
				this.fCopyAction);

		this.fOriginalViewImage = getTitleImage();
		this.fProgressImages = new ProgressImages();
		PlatformUI.getWorkbench().getHelpSystem().setHelp(parent,
				IGUnitHelpContextIds.RESULTS_VIEW);

		getViewSite().getPage().addPartListener(this.fPartListener);

		setFilterAndLayout(false, LAYOUT_HIERARCHICAL);
		if (this.fMemento != null) {
			restoreLayoutState(this.fMemento);
		}
		this.fMemento = null;

		this.fTestRunSessionListener = new TestRunSessionListener();
		GUnitPlugin.getModel().addTestRunSessionListener(
				this.fTestRunSessionListener);
	}

	private void addResizeListener(final Composite parent) {
		parent.addControlListener(new ControlListener() {
			public void controlMoved(final ControlEvent e) {
			}

			public void controlResized(final ControlEvent e) {
				computeOrientation();
			}
		});
	}

	void computeOrientation() {
		if (this.fOrientation != VIEW_ORIENTATION_AUTOMATIC) {
			this.fCurrentOrientation = this.fOrientation;
			setOrientation(this.fCurrentOrientation);
		} else {
			final Point size = this.fParent.getSize();
			if (size.x != 0 && size.y != 0) {
				if (size.x > size.y) {
					setOrientation(VIEW_ORIENTATION_HORIZONTAL);
				} else {
					setOrientation(VIEW_ORIENTATION_VERTICAL);
				}
			}
		}
	}

	private void configureToolBar() {
		final IActionBars actionBars = getViewSite().getActionBars();
		final IToolBarManager toolBar = actionBars.getToolBarManager();
		final IMenuManager viewMenu = actionBars.getMenuManager();

		this.fNextAction = new ShowNextFailureAction(this);
		this.fNextAction.setEnabled(false);
		actionBars.setGlobalActionHandler(ActionFactory.NEXT.getId(),
				this.fNextAction);

		this.fPreviousAction = new ShowPreviousFailureAction(this);
		this.fPreviousAction.setEnabled(false);
		actionBars.setGlobalActionHandler(ActionFactory.PREVIOUS.getId(),
				this.fPreviousAction);

		this.fStopAction = new StopAction();
		this.fStopAction.setEnabled(false);

		this.fRerunLastTestAction = new RerunLastAction();
		final IHandlerService handlerService = (IHandlerService) getSite()
		.getWorkbenchWindow().getService(IHandlerService.class);
		IHandler handler = new AbstractHandler() {
			public Object execute(final ExecutionEvent event)
			throws ExecutionException {
				TestRunnerViewPart.this.fRerunLastTestAction.run();
				return null;
			}

			@Override
			public boolean isEnabled() {
				return TestRunnerViewPart.this.fRerunLastTestAction.isEnabled();
			}
		};
		this.fRerunLastActivation = handlerService.activateHandler(
				RERUN_LAST_COMMAND, handler);

		this.fRerunFailedFirstAction = new RerunLastFailedFirstAction();
		handler = new AbstractHandler() {
			public Object execute(final ExecutionEvent event)
			throws ExecutionException {
				TestRunnerViewPart.this.fRerunFailedFirstAction.run();
				return null;
			}

			@Override
			public boolean isEnabled() {
				return TestRunnerViewPart.this.fRerunFailedFirstAction
				.isEnabled();
			}
		};
		this.fRerunFailedFirstActivation = handlerService.activateHandler(
				RERUN_FAILED_FIRST_COMMAND, handler);

		this.fFailuresOnlyFilterAction = new FailuresOnlyFilterAction();

		this.fScrollLockAction = new ScrollLockAction(this);
		this.fScrollLockAction.setChecked(!this.fAutoScroll);

		this.fToggleOrientationActions = new ToggleOrientationAction[] {
				new ToggleOrientationAction(this, VIEW_ORIENTATION_VERTICAL),
				new ToggleOrientationAction(this, VIEW_ORIENTATION_HORIZONTAL),
				new ToggleOrientationAction(this, VIEW_ORIENTATION_AUTOMATIC) };

		this.fShowTestHierarchyAction = new ShowTestHierarchyAction();

		toolBar.add(this.fNextAction);
		toolBar.add(this.fPreviousAction);
		toolBar.add(this.fFailuresOnlyFilterAction);
		toolBar.add(this.fScrollLockAction);
		toolBar.add(new Separator());
		toolBar.add(this.fRerunLastTestAction);
		toolBar.add(this.fRerunFailedFirstAction);
		toolBar.add(this.fStopAction);
		// toolBar.add(fViewHistory.createHistoryDropDownAction());

		viewMenu.add(this.fShowTestHierarchyAction);
		viewMenu.add(new Separator());

		final MenuManager layoutSubMenu = new MenuManager(
				GUnitMessages.TestRunnerViewPart_layout_menu);
		for (int i = 0; i < this.fToggleOrientationActions.length; ++i) {
			layoutSubMenu.add(this.fToggleOrientationActions[i]);
		}
		viewMenu.add(layoutSubMenu);
		viewMenu.add(new Separator());

		viewMenu.add(this.fFailuresOnlyFilterAction);

		this.fActivateOnErrorAction = new ActivateOnErrorAction();
		viewMenu.add(this.fActivateOnErrorAction);
		this.fViewMenuListener = new IMenuListener() {
			public void menuAboutToShow(final IMenuManager manager) {
				TestRunnerViewPart.this.fActivateOnErrorAction.update();
			}
		};

		viewMenu.addMenuListener(this.fViewMenuListener);

		actionBars.updateActionBars();
	}

	private IStatusLineManager getStatusLine() {
		// we want to show messages globally hence we
		// have to go through the active part
		final IViewSite site = getViewSite();
		final IWorkbenchPage page = site.getPage();
		final IWorkbenchPart activePart = page.getActivePart();

		if (activePart instanceof IViewPart) {
			final IViewPart activeViewPart = (IViewPart) activePart;
			final IViewSite activeViewSite = activeViewPart.getViewSite();
			return activeViewSite.getActionBars().getStatusLineManager();
		}

		if (activePart instanceof IEditorPart) {
			final IEditorPart activeEditorPart = (IEditorPart) activePart;
			final IEditorActionBarContributor contributor = activeEditorPart
			.getEditorSite().getActionBarContributor();
			if (contributor instanceof EditorActionBarContributor) {
				return ((EditorActionBarContributor) contributor)
				.getActionBars().getStatusLineManager();
			}
		}
		// no active part
		return getViewSite().getActionBars().getStatusLineManager();
	}

	protected Composite createProgressCountPanel(final Composite parent) {
		final Composite composite = new Composite(parent, SWT.NONE);
		final GridLayout layout = new GridLayout();
		composite.setLayout(layout);
		setCounterColumns(layout);

		this.fCounterPanel = new CounterPanel(composite);
		this.fCounterPanel.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
				| GridData.HORIZONTAL_ALIGN_FILL));
		this.fProgressBar = new GUnitProgressBar(composite);
		this.fProgressBar.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
				| GridData.HORIZONTAL_ALIGN_FILL));
		return composite;
	}

	public void handleTestSelected(final TestElement test) {
		showFailure(test);
		this.fCopyAction.handleTestSelected(test);
	}

	private void showFailure(final TestElement test) {
		postSyncRunnable(new Runnable() {
			public void run() {
				if (!isDisposed()) {
					TestRunnerViewPart.this.fFailureTrace.showFailure(test);
				}
			}
		});
	}

	/**
	 * @return the Java project, or <code>null</code>
	 */
	public IErlProject getLaunchedProject() {
		return this.fTestRunSession == null ? null : this.fTestRunSession
				.getLaunchedProject();
	}

	public static Image createImage(final String path) {
		return GUnitPlugin.getImageDescriptor(path).createImage();
	}

	private boolean isDisposed() {
		return this.fIsDisposed || this.fCounterPanel.isDisposed();
	}

	private Display getDisplay() {
		return getViewSite().getShell().getDisplay();
	}

	/*
	 * @see IWorkbenchPart#getTitleImage()
	 */
	@Override
	public Image getTitleImage() {
		if (this.fOriginalViewImage == null) {
			this.fOriginalViewImage = super.getTitleImage();
		}

		if (this.fViewImage == null) {
			return super.getTitleImage();
		}
		return this.fViewImage;
	}

	void codeHasChanged() {
		if (this.fDirtyListener != null) {
			ErlangCore.getModelManager().removeElementChangedListener(
					this.fDirtyListener);
			this.fDirtyListener = null;
		}
		if (this.fViewImage == this.fTestRunOKIcon) {
			this.fViewImage = this.fTestRunOKDirtyIcon;
		} else if (this.fViewImage == this.fTestRunFailIcon) {
			this.fViewImage = this.fTestRunFailDirtyIcon;
		}

		final Runnable r = new Runnable() {
			public void run() {
				if (isDisposed()) {
					return;
				}
				firePropertyChange(IWorkbenchPart.PROP_TITLE);
			}
		};
		if (!isDisposed()) {
			getDisplay().asyncExec(r);
		}
	}

	public boolean isCreated() {
		return this.fCounterPanel != null;
	}

	public void rerunTest(final String testId, final String className, final String testName,
			final String launchMode) {
		try {
			final boolean couldLaunch = this.fTestRunSession.rerunTest(testId,
					className, testName, launchMode);
			if (!couldLaunch) {
				MessageDialog.openInformation(getSite().getShell(),
						GUnitMessages.TestRunnerViewPart_cannotrerun_title,
						GUnitMessages.TestRunnerViewPart_cannotrerurn_message);
			} else if (this.fTestRunSession.isKeptAlive()) {
				final TestCaseElement testCaseElement = (TestCaseElement) this.fTestRunSession
				.getTestElement(testId);
				testCaseElement.setStatus(TestElement.Status.RUNNING, null,
						null, null);
				this.fTestViewer.registerViewerUpdate(testCaseElement);
				postSyncProcessChanges();
			}

		} catch (final CoreException e) {
			ErrorDialog.openError(getSite().getShell(),
					GUnitMessages.TestRunnerViewPart_error_cannotrerun, e
					.getMessage(), e.getStatus());
		}
	}

	private void postSyncProcessChanges() {
		postSyncRunnable(new Runnable() {
			public void run() {
				processChangesInUI();
			}
		});
	}

	public void warnOfContentChange() {
		final IWorkbenchSiteProgressService service = getProgressService();
		if (service != null) {
			service.warnOfContentChange();
		}
	}

	public boolean lastLaunchIsKeptAlive() {
		return this.fTestRunSession != null
		&& this.fTestRunSession.isKeptAlive();
	}

	private void setOrientation(final int orientation) {
		if ((this.fSashForm == null) || this.fSashForm.isDisposed()) {
			return;
		}
		final boolean horizontal = orientation == VIEW_ORIENTATION_HORIZONTAL;
		this.fSashForm.setOrientation(horizontal ? SWT.HORIZONTAL
				: SWT.VERTICAL);
		for (int i = 0; i < this.fToggleOrientationActions.length; ++i) {
			this.fToggleOrientationActions[i]
			                               .setChecked(this.fOrientation == this.fToggleOrientationActions[i]
			                                                                                               .getOrientation());
		}
		this.fCurrentOrientation = orientation;
		final GridLayout layout = (GridLayout) this.fCounterComposite.getLayout();
		setCounterColumns(layout);
		this.fParent.layout();
	}

	private void setCounterColumns(final GridLayout layout) {
		if (this.fCurrentOrientation == VIEW_ORIENTATION_HORIZONTAL) {
			layout.numColumns = 2;
		} else {
			layout.numColumns = 1;
		}
	}

	private static boolean getShowOnErrorOnly() {
		final IPreferenceStore store = GUnitPlugin.getDefault().getPreferenceStore();
		return store.getBoolean(GUnitPreferencesConstants.SHOW_ON_ERROR_ONLY);
	}

	public FailureTrace getFailureTrace() {
		return this.fFailureTrace;
	}

	void setShowFailuresOnly(final boolean failuresOnly) {
		setFilterAndLayout(failuresOnly, this.fLayout);
	}

	private void setLayoutMode(final int mode) {
		setFilterAndLayout(this.fFailuresOnlyFilterAction.isChecked(), mode);
	}

	private void setFilterAndLayout(final boolean failuresOnly, final int layoutMode) {
		this.fShowTestHierarchyAction
		.setChecked(layoutMode == LAYOUT_HIERARCHICAL);
		this.fLayout = layoutMode;
		this.fFailuresOnlyFilterAction.setChecked(failuresOnly);
		this.fTestViewer.setShowFailuresOnly(failuresOnly, layoutMode);
	}

	TestElement[] getAllFailures() {
		return this.fTestRunSession.getAllFailedTestElements();
	}
}
