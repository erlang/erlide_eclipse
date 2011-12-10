package org.erlide.debug.ui.actions;

import org.eclipse.debug.core.model.IDebugTarget;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.erlide.debug.ui.tracing.DebugTraceLaunching;
import org.erlide.debug.ui.views.DebuggerTraceView;

public class DebugTraceAsLaunchAction extends Action implements
        IViewActionDelegate {

    private IDebugTarget fTarget;
    private DebuggerTraceView fView;

    @Override
    public void init(final IViewPart view) {
        fView = (DebuggerTraceView) view;
    }

    @Override
    public void run(final IAction action) {
        DebugTraceLaunching.launch(fTarget, fView);
    }

    @Override
    public void selectionChanged(final IAction action,
            final ISelection selection) {
        fTarget = null;
        if (selection instanceof IStructuredSelection) {
            final IStructuredSelection ss = (IStructuredSelection) selection;
            for (final Object o : ss.toArray()) {
                if (o instanceof IDebugTarget) {
                    final IDebugTarget target = (IDebugTarget) o;
                    fTarget = target;
                    break;
                }
            }
        }
    }
}
