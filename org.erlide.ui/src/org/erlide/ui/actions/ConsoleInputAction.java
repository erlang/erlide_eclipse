/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.erlide.ui.views.console.ConsoleInputDialog;

public class ConsoleInputAction implements IWorkbenchWindowActionDelegate {

	private IWorkbenchWindow fWindow;

	public void dispose() {
	}

	public void init(IWorkbenchWindow window) {
		fWindow = window;
	}

	public void run(IAction action) {
		ConsoleInputDialog box = new ConsoleInputDialog(fWindow.getShell(),
				SWT.APPLICATION_MODAL);
		box.setText("txt");
		box.open();
	}

	public void selectionChanged(IAction action, ISelection selection) {
	}

}
