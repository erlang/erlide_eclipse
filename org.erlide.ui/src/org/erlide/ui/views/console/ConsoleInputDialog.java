/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.views.console;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

public class ConsoleInputDialog extends Dialog {

	private Combo combo;

	private StyledText styledText;

	protected Object result;

	protected Shell shell;

	/**
	 * Create the dialog
	 * 
	 * @param parent
	 * @param style
	 */
	public ConsoleInputDialog(Shell parent, int style) {
		super(parent, style);
	}

	/**
	 * Create the dialog
	 * 
	 * @param parent
	 */
	public ConsoleInputDialog(Shell parent) {
		this(parent, SWT.NONE);
	}

	/**
	 * Open the dialog
	 * 
	 * @return the result
	 */
	public Object open() {
		createContents();
		Rectangle p = shell.getParent().getBounds();
		Rectangle s = shell.getBounds();
		int x = p.x + (p.width - s.width) / 2;
		int y = p.y + (p.height - s.height) / 2;
		shell.setLocation(x, y);
		shell.open();
		shell.layout();
		Display display = getParent().getDisplay();
		while (!shell.isDisposed()) {
			if (!display.readAndDispatch()) {
				display.sleep();
			}
		}
		return result;
	}

	/**
	 * Create contents of the dialog
	 */
	protected void createContents() {
		shell = new Shell(getParent(), SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL);
		shell.setSize(500, 217);
		shell.setText("Erlang console input");

		final Label inputLabel = new Label(shell, SWT.NONE);
		inputLabel.setText("Input:");
		inputLabel.setBounds(10, 0, 37, 15);

		styledText = new StyledText(shell, SWT.BORDER);
		styledText.setBounds(10, 21, 472, 138);

		combo = new Combo(shell, SWT.NONE);
		combo.setBounds(10, 165, 471, 21);
		//
	}

}
