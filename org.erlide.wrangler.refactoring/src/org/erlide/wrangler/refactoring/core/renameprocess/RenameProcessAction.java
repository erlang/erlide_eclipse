package org.erlide.wrangler.refactoring.core.renameprocess;

import org.eclipse.ltk.ui.refactoring.RefactoringWizard;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringAction;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringWizard;

public class RenameProcessAction extends WranglerRefactoringAction {

	@Override
	protected String initRefactoringName() {
		return "Rename process";
	}

	@Override
	protected WranglerRefactoring initWranglerRefactoring() {
		return new RenameProcessRefactoring(parameters);
	}

	@Override
	protected WranglerRefactoringWizard initWranglerRefactoringWizard() {
		return new RenameProcessWizard(refactoring,
				RefactoringWizard.DIALOG_BASED_USER_INTERFACE);
	}

}
