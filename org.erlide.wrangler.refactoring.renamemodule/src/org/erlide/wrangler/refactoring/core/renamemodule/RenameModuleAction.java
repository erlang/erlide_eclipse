package org.erlide.wrangler.refactoring.core.renamemodule;

import org.eclipse.ltk.ui.refactoring.RefactoringWizard;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringAction;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringWizard;

public class RenameModuleAction extends WranglerRefactoringAction {

	@Override
	protected String initRefactoringName() {
		return "Rename module";
	}

	@Override
	protected WranglerRefactoring initWranglerRefactoring() {
		return new RenameModuleRefactoring(parameters);
	}

	@Override
	protected WranglerRefactoringWizard initWranglerRefactoringWizard() {
		return new RenameModuleWizard(refactoring,
				RefactoringWizard.DIALOG_BASED_USER_INTERFACE);
	}

}
