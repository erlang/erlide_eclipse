package org.erlide.wrangler.refactoring.ui;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

public abstract class AbstractWranglerAction implements IEditorActionDelegate,
		IWorkbenchWindowActionDelegate {

	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		// GlobalParameters.setEditor(targetEditor);

	}

	public void selectionChanged(IAction action, ISelection selection) {
		// GlobalParameters.setSelection(selection);

	}

	public void dispose() {
	}

	public void init(IWorkbenchWindow window) {
	}

}
