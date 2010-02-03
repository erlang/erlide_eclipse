package org.erlide.wrangler.refactoring.tmp;

import org.eclipse.jface.action.IAction;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.ui.refactoring.RefactoringWizard;
import org.eclipse.ltk.ui.refactoring.RefactoringWizardOpenOperation;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.ui.AbstractWranglerAction;
import org.erlide.wrangler.refactoring.ui.wizard.DefaultWranglerRefactoringWizard;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

public class GeneraliseRefactoringAction extends AbstractWranglerAction {

	public void run(IAction action) {
		GlobalParameters.setSelection(PlatformUI.getWorkbench()
				.getActiveWorkbenchWindow().getActivePage().getSelection());

		// TODO: the final refactoring status
		runWizard(new RefactoringStatus());
	}

	protected void runWizard(RefactoringStatus status) {
		WranglerRefactoring refactoring = new FakeRefactoring(
				"Generalise function", status);
		DefaultWranglerRefactoringWizard wizard = new DefaultWranglerRefactoringWizard(
				refactoring, RefactoringWizard.DIALOG_BASED_USER_INTERFACE,
				null);

		Shell shell = new Shell();
		RefactoringWizardOpenOperation op = new RefactoringWizardOpenOperation(
				wizard);

		try {
			op.run(shell, refactoring.getName());
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
