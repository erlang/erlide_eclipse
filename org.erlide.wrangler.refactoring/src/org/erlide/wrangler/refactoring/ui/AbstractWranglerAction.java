package org.erlide.wrangler.refactoring.ui;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

public abstract class AbstractWranglerAction implements
		IEditorActionDelegate, IWorkbenchWindowActionDelegate {

	@Override
	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		// GlobalParameters.setEditor(targetEditor);

	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		// GlobalParameters.setSelection(selection);

	}

	@Override
	public void dispose() {
	}

	@Override
	public void init(IWorkbenchWindow window) {
	}

}
