package org.erlide.debug.ui.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.erlide.runtime.ErlLogger;

public class ToggleDebugTracingAction implements IWorkbenchWindowActionDelegate {

	public void dispose() {
		// TODO Auto-generated method stub

	}

	public void init(final IWorkbenchWindow window) {
		int i = 0;
		++i;
	}

	public void run(final IAction action) {
		// TODO Auto-generated method stub

	}

	public void selectionChanged(final IAction action,
			final ISelection selection) {
		ErlLogger.info("selectionChanged action " + action + "   sel "
				+ selection);
	}

}
