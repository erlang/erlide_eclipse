package org.erlide.gunit.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * A panel with counters for the number of Runs, Errors and Failures.
 */
public class CounterPanel extends Composite {
	// protected Text fNumberOfErrors;
	protected Text fNumberOfFailures;

	protected Text fNumberOfRuns;

	protected int fTotal;

	protected int fIgnoredCount;

	private final Image fErrorIcon = null; // =

	// TestRunnerViewPart.createImage("ovr16/error_ovr.gif");
	private final Image fFailureIcon = null; // =

	// TestRunnerViewPart.createImage("ovr16/failed_ovr.gif");

	public CounterPanel(Composite parent) {
		super(parent, SWT.WRAP);
		GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 9;
		gridLayout.makeColumnsEqualWidth = false;
		gridLayout.marginWidth = 0;
		setLayout(gridLayout);

		fNumberOfRuns = createLabel("Runs:", null, " 0/0  "); //$NON-NLS-1$
		//fNumberOfErrors= createLabel("Errors:", fErrorIcon, " 0 "); //$NON-NLS-1$
		fNumberOfFailures = createLabel("Failures:", fFailureIcon, " 0 "); //$NON-NLS-1$

		addDisposeListener(new DisposeListener() {
			public void widgetDisposed(DisposeEvent e) {
				disposeIcons();
			}
		});
	}

	void disposeIcons() {
		if (fErrorIcon != null) {
			fErrorIcon.dispose();
		}
		if (fFailureIcon != null) {
			fFailureIcon.dispose();
		}
	}

	private Text createLabel(String name, Image image, String init) {
		Label label = new Label(this, SWT.NONE);
		if (image != null) {
			image.setBackground(label.getBackground());
			label.setImage(image);
		}
		label.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING));

		label = new Label(this, SWT.NONE);
		label.setText(name);
		label.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING));
		// label.setFont(JFaceResources.getBannerFont());

		Text value = new Text(this, SWT.READ_ONLY);
		value.setText(init);
		// bug: 39661 Junit test counters do not repaint correctly [JUnit]
		value.setBackground(getDisplay().getSystemColor(
				SWT.COLOR_WIDGET_BACKGROUND));
		value.setLayoutData(new GridData(GridData.FILL_HORIZONTAL
				| GridData.HORIZONTAL_ALIGN_BEGINNING));
		return value;
	}

	public void reset() {
		// setErrorValue(0);
		setFailureValue(0);
		setRunValue(0, 0);
		fTotal = 0;
	}

	public void setTotal(int value) {
		fTotal = value;
	}

	public int getTotal() {
		return fTotal;
	}

	public void setRunValue(int value, int ignoredCount) {
		String runString = Integer.toString(value) + "/"
				+ Integer.toString(fTotal);
		// if (ignoredCount == 0)
		// runString= Messages.format(JUnitMessages.CounterPanel_runcount, new
		// String[] { Integer.toString(value), Integer.toString(fTotal) });
		// else
		// runString=
		// Messages.format(JUnitMessages.CounterPanel_runcount_ignored, new
		// String[] { Integer.toString(value), Integer.toString(fTotal),
		// Integer.toString(ignoredCount) });
		fNumberOfRuns.setText(runString);

		if (fIgnoredCount == 0 && ignoredCount > 0 || fIgnoredCount != 0
				&& ignoredCount == 0) {
			layout();
		} else {
			fNumberOfRuns.redraw();
			redraw();
		}
		fIgnoredCount = ignoredCount;
	}

	// public void setErrorValue(int value) {
	// fNumberOfErrors.setText(Integer.toString(value));
	// redraw();
	// }

	public void setFailureValue(int value) {
		fNumberOfFailures.setText(Integer.toString(value));
		redraw();
	}
}
