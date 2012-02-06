package org.erlide.debug.ui.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.erlide.launch.debug.model.ErlangProcess;

public class ToggleDebugTracingAction implements IWorkbenchWindowActionDelegate {

    private ISelection fSelection;

    @Override
    public void dispose() {
    }

    @Override
    public void init(final IWorkbenchWindow window) {
    }

    @Override
    public void run(final IAction action) {
        final IStructuredSelection ss = (IStructuredSelection) fSelection;
        for (final Object o : ss.toArray()) {
            if (o instanceof ErlangProcess) {
                final ErlangProcess p = (ErlangProcess) o;
                p.setTracing(!p.getTracing());
            }
        }
    }

    @Override
    public void selectionChanged(final IAction action,
            final ISelection selection) {
        fSelection = selection;
    }

}
