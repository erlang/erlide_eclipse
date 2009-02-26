/*******************************************************************************
 * Copyright (c) 2005, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
/**
 * 
 */
package org.erlide.gunit.internal.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;

public class FailureTableDisplay implements ITraceDisplay {
	private final Table fTable;

	private final Image fExceptionIcon = TestRunnerViewPart
			.createImage("obj16/exc_catch.gif"); //$NON-NLS-1$

	private final Image fStackIcon = TestRunnerViewPart
			.createImage("obj16/stkfrm_obj.gif"); //$NON-NLS-1$

	public FailureTableDisplay(Table table) {
		fTable = table;
		fTable.getParent().addDisposeListener(new DisposeListener() {
			public void widgetDisposed(DisposeEvent e) {
				disposeIcons();
			}
		});
	}

	public void addTraceLine(int lineType, String label) {
		TableItem tableItem = newTableItem();
		switch (lineType) {
		case TextualTrace.LINE_TYPE_EXCEPTION:
			tableItem.setImage(fExceptionIcon);
			break;
		case TextualTrace.LINE_TYPE_STACKFRAME:
			tableItem.setImage(fStackIcon);
			break;
		case TextualTrace.LINE_TYPE_NORMAL:
		default:
			break;
		}
		tableItem.setText(label);
	}

	public Image getExceptionIcon() {
		return fExceptionIcon;
	}

	public Image getStackIcon() {
		return fStackIcon;
	}

	public Table getTable() {
		return fTable;
	}

	private void disposeIcons() {
		if (fExceptionIcon != null && !fExceptionIcon.isDisposed())
			fExceptionIcon.dispose();
		if (fStackIcon != null && !fStackIcon.isDisposed())
			fStackIcon.dispose();
	}

	TableItem newTableItem() {
		return new TableItem(fTable, SWT.NONE);
	}
}
