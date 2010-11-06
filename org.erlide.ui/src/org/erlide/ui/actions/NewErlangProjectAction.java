package org.erlide.ui.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.erlide.ui.wizards.NewErlangProject;

public class NewErlangProjectAction implements IViewActionDelegate {

    private final IStructuredSelection selection = StructuredSelection.EMPTY;

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
     */
    public void init(final IViewPart view) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
     */
    public void run(final IAction action) {
        // Create the Wizard
        final NewErlangProject wizard = new NewErlangProject();
        wizard.init(getWorkbench(), selection);

        // Create the wizard dialog
        final Shell shell = getWorkbench().getActiveWorkbenchWindow()
                .getShell();
        final WizardDialog dialog = new WizardDialog(shell, wizard);
        // Open the wizard dialog
        dialog.open();

    }

    /**
     * @return
     */
    private IWorkbench getWorkbench() {
        return PlatformUI.getWorkbench();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action
     * .IAction, org.eclipse.jface.viewers.ISelection)
     */
    public void selectionChanged(final IAction action,
            final ISelection aSelection) {
        // TODO Auto-generated method stub
    }

}
