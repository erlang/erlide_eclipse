package org.erlide.gunit.internal.ui;

import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.custom.ViewForm;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.ui.part.ViewPart;
import org.erlide.gunit.GUnitPlugin;
import org.erlide.gunit.model.ITestRunSessionListener;
import org.erlide.gunit.model.ITestSessionListener;
import org.erlide.gunit.model.TestElement;
import org.erlide.gunit.model.TestRunSession;

public class TestRunnerViewPart extends ViewPart {

	public static String VIEW_ID = "org.erlide.gunit.ui.TestRunnerViewPart"; //$NON-NLS-1$

	public static final Image fTestIcon = TestRunnerViewPart
			.createImage("test.gif"); //$NON-NLS-1$

	public static final Image fTestOkIcon = TestRunnerViewPart
			.createImage("testok.gif"); //$NON-NLS-1$

	public static final Image fTestFailIcon = TestRunnerViewPart
			.createImage("testfail.gif"); //$NON-NLS-1$

	public static final Image fTestRunningIcon = TestRunnerViewPart
			.createImage("testrun.gif"); //$NON-NLS-1$

	private SashForm fSashForm;

	private Composite fCounterComposite;

	protected ProgressBar fProgressBar;

	protected CounterPanel fCounterPanel;

	protected TestViewer fTestViewer;

	protected StyledText fFailureTrace;

	TestRunSession fTestRunSession;

	private TestSessionListener fTestSessionListener;

	private TestRunSessionListener fTestRunSessionListener;

	class TestRunSessionListener implements ITestRunSessionListener {

		public void sessionAdded(TestRunSession testRunSession) {
			setActiveTestRunSession(testRunSession);
		}

		public void sessionRemoved(TestRunSession testRunSession) {
			// TODO Auto-generated method stub

		}

	}

	void update() {
		getDisplay().asyncExec(new Runnable() {

			public void run() {
				fTestViewer.refresh();
				fCounterPanel.setTotal(fTestRunSession.getTotalCount());
				fCounterPanel.setRunValue(fTestRunSession.getEndedCount(), 0);
				fCounterPanel
						.setFailureValue(fTestRunSession.getFailureCount());
				fProgressBar.reset(fTestRunSession.getFailureCount() > 0,
						false, fTestRunSession.getEndedCount(), fTestRunSession
								.getTotalCount());
			}

		});
	}

	class TestSessionListener implements ITestSessionListener {

		public void runningBegins() {
			// getDisplay().asyncExec(new Runnable() {
			//
			// public void run() {
			// fTestViewer.setInput(fTestRunSession.getTestElements());
			// }
			//
			// });
			// update();
		}

		public void sessionEnded(long elapsedTime) {
			// System.out.println("elapsedTime = " + (float)elapsedTime/1000f +
			// " seconds");
		}

		public void sessionStarted() {
			// fCounterPanel.setTotal(fTestRunSession.getTotalCount());
		}

		public void sessionStopped(long elapsedTime) {
			// TODO Auto-generated method stub

		}

		public void sessionTerminated() {
			// TODO Auto-generated method stub

		}

		public void testAdded(TestElement testElement) {
			// TODO Auto-generated method stub

		}

		public void testEnded(TestElement testCaseElement) {
			update();
		}

		public void testFailed(TestElement testElement, String trace,
				String expected, String actual) {
			update();

		}

		public void testStarted(TestElement testCaseElement) {
			update();
		}

	}

	public TestRunnerViewPart() {
		// TODO Auto-generated constructor stub
	}

	public void setActiveTestRunSession(TestRunSession testRunSession) {
		fTestRunSession = testRunSession;

		getDisplay().asyncExec(new Runnable() {

			public void run() {
				fTestViewer.setInput(fTestRunSession.getTestElements());
			}

		});

		fTestSessionListener = new TestSessionListener();
		fTestRunSession.addTestSessionListener(fTestSessionListener);
	}

	@Override
	public void createPartControl(Composite parent) {

		GridLayout gridLayout = new GridLayout();
		gridLayout.marginWidth = 0;
		gridLayout.marginHeight = 0;
		parent.setLayout(gridLayout);

		fCounterComposite = createProgressCountPanel(parent);
		fCounterComposite.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
				| GridData.HORIZONTAL_ALIGN_FILL));
		SashForm sashForm = createSashForm(parent);
		sashForm.setLayoutData(new GridData(GridData.FILL_BOTH));

		fTestRunSessionListener = new TestRunSessionListener();
		GUnitPlugin.getModel().addBTErlTestRunSessionListener(
				fTestRunSessionListener);
		initializeToolBar();
	}

	protected Composite createProgressCountPanel(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout();
		composite.setLayout(layout);
		layout.numColumns = 1;

		fCounterPanel = new CounterPanel(composite);
		fCounterPanel.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
				| GridData.HORIZONTAL_ALIGN_FILL));
		fProgressBar = new ProgressBar(composite);
		fProgressBar.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
				| GridData.HORIZONTAL_ALIGN_FILL));
		return composite;
	}

	private SashForm createSashForm(Composite parent) {
		fSashForm = new SashForm(parent, SWT.VERTICAL);

		ViewForm top = new ViewForm(fSashForm, SWT.NONE);

		Composite empty = new Composite(top, SWT.NONE);
		empty.setLayout(new Layout() {
			@Override
			protected Point computeSize(Composite composite, int wHint,
					int hHint, boolean flushCache) {
				return new Point(1, 1); // (0, 0) does not work with
				// super-intelligent ViewForm
			}

			@Override
			protected void layout(Composite composite, boolean flushCache) {
			}
		});
		top.setTopLeft(empty); // makes ViewForm draw the horizontal separator
		// line ...
		// fTestViewer= new TestViewer(top, fClipboard, this);
		// fTestViewer= new StyledText(top, SWT.MULTI);
		fTestViewer = new TestViewer(top);
		// fTestViewer.setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_DARK_RED));
		top.setContent(fTestViewer.getControl());

		ViewForm bottom = new ViewForm(fSashForm, SWT.NONE);

		CLabel label = new CLabel(bottom, SWT.NONE);
		label.setText("Failure Trace");
		// label.setImage(fStackViewIcon);
		bottom.setTopLeft(label);
		ToolBar failureToolBar = new ToolBar(bottom, SWT.FLAT | SWT.WRAP);
		bottom.setTopCenter(failureToolBar);
		// fFailureTrace= new FailureTrace(bottom, fClipboard, this,
		// failureToolBar);
		fFailureTrace = new StyledText(bottom, SWT.MULTI);
		bottom.setContent(fFailureTrace);

		fSashForm.setWeights(new int[] { 50, 50 });
		return fSashForm;
	}

	public boolean isCreated() {
		return fCounterPanel != null;
	}

	@Override
	public void setFocus() {
		// TODO Auto-generated method stub

	}

	public CounterPanel getCounterPanel() {
		return fCounterPanel;
	}

	public ProgressBar getProgressBar() {
		return fProgressBar;
	}

	public void testProgress(final int total, final int testsOk,
			final int testsFailed) {

		Display.getDefault().asyncExec(new Runnable() {

			public void run() {
				fCounterPanel.setTotal(total);
				fCounterPanel.setRunValue(testsOk + testsFailed, 0);
				fCounterPanel.setFailureValue(testsFailed);
				fProgressBar.reset(testsFailed > 0, false, testsOk
						+ testsFailed, total);
			}

		});
	}

	private Display getDisplay() {
		Display display = Display.getCurrent();
		if (display == null) {
			display = Display.getDefault();
		}
		return display;
	}

	public static Image createImage(String path) {
		return GUnitPlugin.getImageDescriptor(path).createImage();
	}

	private void initializeToolBar() {
		IToolBarManager toolBarManager = getViewSite().getActionBars()
				.getToolBarManager();
	}
}
