package org.erlide.wrangler.refactoring.core.renamevariable;

import org.eclipse.ltk.ui.refactoring.RefactoringWizard;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringAction;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringWizard;

public class RenameVariableAction extends WranglerRefactoringAction {

	@Override
	public void dispose() {
	}

	@Override
	protected WranglerRefactoring initWranglerRefactoring() {
		return new RenameVariableRefactoring(parameters);
	}

	@Override
	protected WranglerRefactoringWizard initWranglerRefactoringWizard() {
		return new RenameVariableWizard(refactoring,
				RefactoringWizard.DIALOG_BASED_USER_INTERFACE);

	}

	@Override
	protected String initRefactoringName() {
		return "Rename variable";
	}

}
