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
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

public class LayoutUtil {

	// /*
	// * Calculates the number of columns needed by field editors
	// */
	// public static int getNumberOfColumns(
	// MethodStubsSelectionButtonGroup[] editors) {
	// int columnCount = 0;
	// for (int i = 0; i < editors.length; i++) {
	// columnCount = Math.max(editors[i].getNumberOfControls(),
	// columnCount);
	// }
	// return columnCount;
	// }
	//
	// /*
	// * Creates a composite and fills in the given editors.
	// *
	// * @param labelOnTop Defines if the label of all fields should be on top
	// of
	// * the fields
	// */
	// public static void doDefaultLayout(Composite parent,
	// MethodStubsSelectionButtonGroup[] editors, boolean labelOnTop) {
	// doDefaultLayout(parent, editors, labelOnTop, 0, 0, 0, 0);
	// }
	//
	// /*
	// * Creates a composite and fills in the given editors.
	// *
	// * @param labelOnTop Defines if the label of all fields should be on top
	// of
	// * the fields
	// *
	// * @param minWidth The minimal width of the composite
	// *
	// * @param minHeight The minimal height of the composite
	// */
	// public static void doDefaultLayout(Composite parent,
	// MethodStubsSelectionButtonGroup[] editors, boolean labelOnTop,
	// int minWidth, int minHeight) {
	// doDefaultLayout(parent, editors, labelOnTop, minWidth, minHeight, 0, 0);
	// }
	//
	// public static void doDefaultLayout(Composite parent,
	// MethodStubsSelectionButtonGroup[] editors, boolean labelOnTop,
	// int minWidth, int minHeight, int marginWidth, int marginHeight) {
	// int nCulumns = getNumberOfColumns(editors);
	// Control[][] controls = new Control[editors.length][];
	// for (int i = 0; i < editors.length; i++) {
	// controls[i] = editors[i].doFillIntoGrid(parent, nCulumns);
	// }
	// if (labelOnTop) {
	// nCulumns--;
	// modifyLabelSpans(controls, nCulumns);
	// }
	// GridLayout layout = new GridLayout();
	// if (marginWidth != SWT.DEFAULT) {
	// layout.marginWidth = marginWidth;
	// }
	// if (marginHeight != SWT.DEFAULT) {
	// layout.marginHeight = marginHeight;
	// }
	// // layout.minimumWidth= minWidth;
	// // layout.minimumHeight= minHeight;
	// layout.numColumns = nCulumns;
	// parent.setLayout(layout);
	// }

	private static void modifyLabelSpans(final Control[][] controls, final int nCulumns) {
		for (int i = 0; i < controls.length; i++) {
			setHorizontalSpan(controls[i][0], nCulumns);
		}
	}

	/*
	 * Sets the span of a control. Assumes that MGridData is used.
	 */
	public static void setHorizontalSpan(final Control control, final int span) {
		final Object ld = control.getLayoutData();
		if (ld instanceof GridData) {
			((GridData) ld).horizontalSpan = span;
		} else if (span != 1) {
			final GridData gd = new GridData();
			gd.horizontalSpan = span;
			control.setLayoutData(gd);
		}
	}

	/*
	 * Sets the width hint of a control. Assumes that MGridData is used.
	 */
	public static void setWidthHint(final Control control, final int widthHint) {
		final Object ld = control.getLayoutData();
		if (ld instanceof GridData) {
			((GridData) ld).widthHint = widthHint;
		}
	}

	/*
	 * Sets the horizontal indent of a control. Assumes that MGridData is used.
	 */
	public static void setHorizontalIndent(final Control control, final int horizontalIndent) {
		final Object ld = control.getLayoutData();
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
	public static Control createEmptySpace(final Composite parent, final int span) {
		final Label label = new Label(parent, SWT.LEFT);
		final GridData gd = new GridData();
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
	public static int getButtonWidthHint(final Button button) {
		button.setFont(JFaceResources.getDialogFont());
		final PixelConverter converter = new PixelConverter(button);
		final int widthHint = converter
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
	public static void setButtonDimensionHint(final Button button) {
		Assert.isNotNull(button);
		final Object gd = button.getLayoutData();
		if (gd instanceof GridData) {
			((GridData) gd).widthHint = getButtonWidthHint(button);
			((GridData) gd).horizontalAlignment = GridData.FILL;
		}
	}

}
