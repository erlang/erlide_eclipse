/*******************************************************************************
 * Copyright (c) 2010 György Orosz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     György Orosz - initial API and implementation
 ******************************************************************************/
package org.erlide.wrangler.refactoring.core;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;

public abstract class RefactoringWorkflowController {
	Shell shell;

	public RefactoringWorkflowController(Shell shell) {
		this.shell = shell;
	}

	public abstract void doRefactoring();

	public void stop() {
		shell.close();
	}

	public boolean ask(String title, String message) {
		return MessageDialog.openQuestion(shell, title, message);
	}

}
