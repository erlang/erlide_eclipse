package org.erlide.debug.ui.actions;

import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.erlide.runtime.debug.ErlangDebugElement;

public class ErlangTracingAction implements IWorkbenchWindowActionDelegate {

    private ILaunch fLaunch;
    private ErlangTracingDialog fDialog;

    public void dispose() {
    }

    public void init(final IWorkbenchWindow window) {
        fDialog = new ErlangTracingDialog(window.getShell());
    }

    public void run(final IAction action) {
        if (fLaunch != null) {
            fDialog.initializeFrom(fLaunch);
            fDialog.open();
        }
    }

    public void selectionChanged(final IAction action,
            final ISelection selection) {
        fLaunch = null;
        if (selection instanceof IStructuredSelection) {
            final IStructuredSelection ss = (IStructuredSelection) selection;
            for (final Object o : ss.toArray()) {
                if (o instanceof ErlangDebugElement) {
                    final ErlangDebugElement d = (ErlangDebugElement) o;
                    fLaunch = d.getLaunch();
                } else if (o instanceof ILaunch) {
                    fLaunch = (ILaunch) o;
                } else if (o instanceof IProcess) {
                    final IProcess p = (IProcess) o;
                    fLaunch = p.getLaunch();
                }
            }
        }
    }

}
