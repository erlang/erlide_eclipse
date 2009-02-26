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
package org.erlide.gunit.internal.util;

import org.eclipse.core.runtime.Assert;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.JFaceResources;

public class LayoutUtil {

	/*
	 * Calculates the number of columns needed by field editors
	 */
	public static int getNumberOfColumns(
			MethodStubsSelectionButtonGroup[] editors) {
		int columnCount = 0;
		for (int i = 0; i < editors.length; i++) {
			columnCount = Math.max(editors[i].getNumberOfControls(),
					columnCount);
		}
		return columnCount;
	}

	/*
	 * Creates a composite and fills in the given editors.
	 * 
	 * @param labelOnTop Defines if the label of all fields should be on top of
	 * the fields
	 */
	public static void doDefaultLayout(Composite parent,
			MethodStubsSelectionButtonGroup[] editors, boolean labelOnTop) {
		doDefaultLayout(parent, editors, labelOnTop, 0, 0, 0, 0);
	}

	/*
	 * Creates a composite and fills in the given editors.
	 * 
	 * @param labelOnTop Defines if the label of all fields should be on top of
	 * the fields
	 * 
	 * @param minWidth The minimal width of the composite
	 * 
	 * @param minHeight The minimal height of the composite
	 */
	public static void doDefaultLayout(Composite parent,
			MethodStubsSelectionButtonGroup[] editors, boolean labelOnTop,
			int minWidth, int minHeight) {
		doDefaultLayout(parent, editors, labelOnTop, minWidth, minHeight, 0, 0);
	}

	public static void doDefaultLayout(Composite parent,
			MethodStubsSelectionButtonGroup[] editors, boolean labelOnTop,
			int minWidth, int minHeight, int marginWidth, int marginHeight) {
		int nCulumns = getNumberOfColumns(editors);
		Control[][] controls = new Control[editors.length][];
		for (int i = 0; i < editors.length; i++) {
			controls[i] = editors[i].doFillIntoGrid(parent, nCulumns);
		}
		if (labelOnTop) {
			nCulumns--;
			modifyLabelSpans(controls, nCulumns);
		}
		GridLayout layout = new GridLayout();
		if (marginWidth != SWT.DEFAULT) {
			layout.marginWidth = marginWidth;
		}
		if (marginHeight != SWT.DEFAULT) {
			layout.marginHeight = marginHeight;
		}
		// layout.minimumWidth= minWidth;
		// layout.minimumHeight= minHeight;
		layout.numColumns = nCulumns;
		parent.setLayout(layout);
	}

	private static void modifyLabelSpans(Control[][] controls, int nCulumns) {
		for (int i = 0; i < controls.length; i++) {
			setHorizontalSpan(controls[i][0], nCulumns);
		}
	}

	/*
	 * Sets the span of a control. Assumes that MGridData is used.
	 */
	public static void setHorizontalSpan(Control control, int span) {
		Object ld = control.getLayoutData();
		if (ld instanceof GridData) {
			((GridData) ld).horizontalSpan = span;
		} else if (span != 1) {
			GridData gd = new GridData();
			gd.horizontalSpan = span;
			control.setLayoutData(gd);
		}
	}

	/*
	 * Sets the width hint of a control. Assumes that MGridData is used.
	 */
	public static void setWidthHint(Control control, int widthHint) {
		Object ld = control.getLayoutData();
		if (ld instanceof GridData) {
			((GridData) ld).widthHint = widthHint;
		}
	}

	/*
	 * Sets the horizontal indent of a control. Assumes that MGridData is used.
	 */
	public static void setHorizontalIndent(Control control, int horizontalIndent) {
		Object ld = control.getLayoutData();
		if (ld instanceof GridData) {
			((GridData) ld).horizontalIndent = horizontalIndent;
		}
	}

	/*
	 * Creates a spacer control with the given span. The composite is assumed to
	 * have <code>MGridLayout</code> as layout.
	 * 
	 * @param parent The parent composite
	 */
	public static Control createEmptySpace(Composite parent, int span) {
		Label label = new Label(parent, SWT.LEFT);
		GridData gd = new GridData();
		gd.horizontalAlignment = GridData.BEGINNING;
		gd.grabExcessHorizontalSpace = false;
		gd.horizontalSpan = span;
		gd.horizontalIndent = 0;
		gd.widthHint = 0;
		gd.heightHint = 0;
		label.setLayoutData(gd);
		return label;
	}

	/**
	 * Returns a width hint for a button control.
	 * 
	 * @param button
	 *            the button for which to set the dimension hint
	 * @return the width hint
	 */
	public static int getButtonWidthHint(Button button) {
		button.setFont(JFaceResources.getDialogFont());
		PixelConverter converter = new PixelConverter(button);
		int widthHint = converter
				.convertHorizontalDLUsToPixels(IDialogConstants.BUTTON_WIDTH);
		return Math.max(widthHint, button.computeSize(SWT.DEFAULT, SWT.DEFAULT,
				true).x);
	}

	/**
	 * Sets width and height hint for the button control. <b>Note:</b> This is a
	 * NOP if the button's layout data is not an instance of
	 * <code>GridData</code>.
	 * 
	 * @param button
	 *            the button for which to set the dimension hint
	 */
	public static void setButtonDimensionHint(Button button) {
		Assert.isNotNull(button);
		Object gd = button.getLayoutData();
		if (gd instanceof GridData) {
			((GridData) gd).widthHint = getButtonWidthHint(button);
			((GridData) gd).horizontalAlignment = GridData.FILL;
		}
	}

}
