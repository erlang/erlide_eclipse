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
