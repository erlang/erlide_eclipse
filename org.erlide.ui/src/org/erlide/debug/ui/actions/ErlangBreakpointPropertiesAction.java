package org.erlide.debug.ui.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionDelegate;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PropertyDialogAction;
import org.erlide.runtime.debug.IErlangBreakpoint;

public class ErlangBreakpointPropertiesAction implements IObjectActionDelegate {

    private IWorkbenchPart fPart;
    IErlangBreakpoint fBreakpoint;

    /**
     * @see IActionDelegate#run(IAction)
     */
    public void run(final IAction action) {
        if (fBreakpoint != null) {
            IShellProvider provider;
            if (fPart != null) {
                provider = fPart.getSite();
            } else {
                provider = new IShellProvider() {
                    public Shell getShell() {
                        final IWorkbench workbench = PlatformUI.getWorkbench();
                        final IWorkbenchWindow window = workbench
                                .getActiveWorkbenchWindow();
                        return window == null ? null : window.getShell();
                    }
                };
            }
            final PropertyDialogAction propertyAction = new PropertyDialogAction(
                    provider, new ISelectionProvider() {
                        public void addSelectionChangedListener(
                                final ISelectionChangedListener listener) {
                        }

                        public ISelection getSelection() {
                            return new StructuredSelection(fBreakpoint);
                        }

                        public void removeSelectionChangedListener(
                                final ISelectionChangedListener listener) {
                        }

                        public void setSelection(final ISelection selection) {
                        }
                    });
            propertyAction.run();
        }
    }

    /**
     * @see IActionDelegate#selectionChanged(IAction, ISelection)
     */
    public void selectionChanged(final IAction action,
            final ISelection selection) {
        if (selection instanceof IStructuredSelection) {
            final IStructuredSelection ss = (IStructuredSelection) selection;
            if (ss.isEmpty() || ss.size() > 1) {
                return;
            }
            final Object element = ss.getFirstElement();
            if (element instanceof IErlangBreakpoint) {
                setBreakpoint((IErlangBreakpoint) element);
            } else {
                setBreakpoint(null);
            }
        }
    }

    /**
     * Allows the underlying breakpoint for the properties page to be set
     * 
     * @param breakpoint
     */
    public void setBreakpoint(final IErlangBreakpoint breakpoint) {
        fBreakpoint = breakpoint;
    }

    /**
     * @see IObjectActionDelegate#setActivePart(IAction, IWorkbenchPart)
     */
    public void setActivePart(final IAction action,
            final IWorkbenchPart targetPart) {
        fPart = targetPart;
    }
}
