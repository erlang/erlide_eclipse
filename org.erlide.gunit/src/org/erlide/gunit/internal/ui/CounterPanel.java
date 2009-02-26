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
package org.erlide.gunit.internal.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.erlide.gunit.internal.Messages;

/**
 * A panel with counters for the number of Runs, Errors and Failures.
 */
public class CounterPanel extends Composite {
	protected Text fNumberOfErrors;

	protected Text fNumberOfFailures;

	protected Text fNumberOfRuns;

	protected int fTotal;

	protected int fIgnoredCount;

	private final Image fErrorIcon = TestRunnerViewPart
			.createImage("ovr16/error_ovr.gif"); //$NON-NLS-1$

	private final Image fFailureIcon = TestRunnerViewPart
			.createImage("ovr16/failed_ovr.gif"); //$NON-NLS-1$

	public CounterPanel(Composite parent) {
		super(parent, SWT.WRAP);
		GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 9;
		gridLayout.makeColumnsEqualWidth = false;
		gridLayout.marginWidth = 0;
		setLayout(gridLayout);

		this.fNumberOfRuns = createLabel(GUnitMessages.CounterPanel_label_runs,
				null, " 0/0  "); //$NON-NLS-1$
		this.fNumberOfErrors = createLabel(
				GUnitMessages.CounterPanel_label_errors, this.fErrorIcon, " 0 "); //$NON-NLS-1$
		this.fNumberOfFailures = createLabel(
				GUnitMessages.CounterPanel_label_failures, this.fFailureIcon,
				" 0 "); //$NON-NLS-1$

		addDisposeListener(new DisposeListener() {
			public void widgetDisposed(DisposeEvent e) {
				disposeIcons();
			}
		});
	}

	private void disposeIcons() {
		this.fErrorIcon.dispose();
		this.fFailureIcon.dispose();
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
		setErrorValue(0);
		setFailureValue(0);
		setRunValue(0, 0);
		this.fTotal = 0;
	}

	public void setTotal(int value) {
		this.fTotal = value;
	}

	public int getTotal() {
		return this.fTotal;
	}

	public void setRunValue(int value, int ignoredCount) {
		String runString;
		if (ignoredCount == 0) {
			runString = Messages.format(GUnitMessages.CounterPanel_runcount,
					new String[] { Integer.toString(value),
							Integer.toString(this.fTotal) });
		} else {
			runString = Messages.format(
					GUnitMessages.CounterPanel_runcount_ignored, new String[] {
							Integer.toString(value),
							Integer.toString(this.fTotal),
							Integer.toString(ignoredCount) });
		}
		this.fNumberOfRuns.setText(runString);

		if (this.fIgnoredCount == 0 && ignoredCount > 0
				|| this.fIgnoredCount != 0 && ignoredCount == 0) {
			layout();
		} else {
			this.fNumberOfRuns.redraw();
			redraw();
		}
		this.fIgnoredCount = ignoredCount;
	}

	public void setErrorValue(int value) {
		this.fNumberOfErrors.setText(Integer.toString(value));
		redraw();
	}

	public void setFailureValue(int value) {
		this.fNumberOfFailures.setText(Integer.toString(value));
		redraw();
	}
}
