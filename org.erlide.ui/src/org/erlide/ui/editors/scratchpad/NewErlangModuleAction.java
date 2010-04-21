package org.erlide.ui.editors.scratchpad;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionDelegate;

public class NewErlangModuleAction extends ActionDelegate {

	private IStructuredSelection selection;

	public NewErlangModuleAction() {
		selection = StructuredSelection.EMPTY;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.actions.ActionDelegate#selectionChanged(org.eclipse.jface
	 * .action.IAction, org.eclipse.jface.viewers.ISelection)
	 */
	@Override
	public void selectionChanged(final IAction action, final ISelection sel) {
		if (sel instanceof IStructuredSelection) {
			selectionChanged((IStructuredSelection) sel);
		} else {
			selection = StructuredSelection.EMPTY;
		}
	}

	@Override
	public void run(final IAction action) {
		// Create the Wizard
		final NewErlangScratchPadWizard wizard = new NewErlangScratchPadWizard();
		wizard.init(getWorkbench(), selection);

		// Create the wizard dialog
		final Shell shell = getWorkbench().getActiveWorkbenchWindow()
				.getShell();
		final WizardDialog dialog = new WizardDialog(shell, wizard);
		// Open the wizard dialog
		dialog.open();
	}

	private IWorkbench getWorkbench() {
		return PlatformUI.getWorkbench();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.actions.SelectionProviderAction#selectionChanged(org.eclipse
	 * .jface.viewers.IStructuredSelection)
	 */
	public void selectionChanged(final IStructuredSelection aSelection) {
		selection = aSelection;
	}

}
