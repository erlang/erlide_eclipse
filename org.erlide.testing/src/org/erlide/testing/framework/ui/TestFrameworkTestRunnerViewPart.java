package org.erlide.testing.framework.ui;

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
import org.erlide.testing.framework.TestFrameworkPlugin;
import org.erlide.testing.framework.model.ITestFrameworkTestRunSessionListener;
import org.erlide.testing.framework.model.ITestFrameworkTestSessionListener;
import org.erlide.testing.framework.model.TestFrameworkTestElement;
import org.erlide.testing.framework.model.TestFrameworkTestRunSession;

public class TestFrameworkTestRunnerViewPart extends ViewPart {

	public static String VIEW_ID = "org.erlide.testing.framework.ui.TestFrameworkTestRunnerViewPart"; //$NON-NLS-1$

	public static final Image fTestIcon = TestFrameworkTestRunnerViewPart
			.createImage("test.gif"); //$NON-NLS-1$
	public static final Image fTestOkIcon = TestFrameworkTestRunnerViewPart
			.createImage("testok.gif"); //$NON-NLS-1$
	public static final Image fTestFailIcon = TestFrameworkTestRunnerViewPart
			.createImage("testfail.gif"); //$NON-NLS-1$
	public static final Image fTestRunningIcon = TestFrameworkTestRunnerViewPart
			.createImage("testrun.gif"); //$NON-NLS-1$

	private SashForm fSashForm;
	private Composite fCounterComposite;
	protected TestFrameworkProgressBar fProgressBar;
	protected TestFrameworkCounterPanel fCounterPanel;
	protected TestFrameworkTestViewer fTestViewer;
	protected StyledText fFailureTrace;

	private TestFrameworkTestRunSession fTestRunSession;
	private BTErlTestSessionListener fTestSessionListener;

	private BTErlTestRunSessionListener fTestRunSessionListener;

	private class BTErlTestRunSessionListener implements
			ITestFrameworkTestRunSessionListener {

		public void sessionAdded(TestFrameworkTestRunSession testRunSession) {
			setActiveTestRunSession(testRunSession);
		}

		public void sessionRemoved(TestFrameworkTestRunSession testRunSession) {
			// TODO Auto-generated method stub

		}

	}

	private void update() {
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

	private class BTErlTestSessionListener implements
			ITestFrameworkTestSessionListener {

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

		public void testAdded(TestFrameworkTestElement testElement) {
			// TODO Auto-generated method stub

		}

		public void testEnded(TestFrameworkTestElement testCaseElement) {
			update();
		}

		public void testFailed(TestFrameworkTestElement testElement,
				String trace, String expected, String actual) {
			update();

		}

		public void testStarted(TestFrameworkTestElement testCaseElement) {
			update();
		}

	}

	public TestFrameworkTestRunnerViewPart() {
		// TODO Auto-generated constructor stub
	}

	public void setActiveTestRunSession(
			TestFrameworkTestRunSession testRunSession) {
		fTestRunSession = testRunSession;

		getDisplay().asyncExec(new Runnable() {

			public void run() {
				fTestViewer.setInput(fTestRunSession.getTestElements());
			}

		});

		fTestSessionListener = new BTErlTestSessionListener();
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

		fTestRunSessionListener = new BTErlTestRunSessionListener();
		TestFrameworkPlugin.getModel().addBTErlTestRunSessionListener(
				fTestRunSessionListener);
	}

	protected Composite createProgressCountPanel(Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout();
		composite.setLayout(layout);
		layout.numColumns = 1;

		fCounterPanel = new TestFrameworkCounterPanel(composite);
		fCounterPanel.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
				| GridData.HORIZONTAL_ALIGN_FILL));
		fProgressBar = new TestFrameworkProgressBar(composite);
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
		fTestViewer = new TestFrameworkTestViewer(top);
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

	public TestFrameworkCounterPanel getCounterPanel() {
		return fCounterPanel;
	}

	public TestFrameworkProgressBar getProgressBar() {
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
		return TestFrameworkPlugin.getImageDescriptor(path).createImage();
	}
}
