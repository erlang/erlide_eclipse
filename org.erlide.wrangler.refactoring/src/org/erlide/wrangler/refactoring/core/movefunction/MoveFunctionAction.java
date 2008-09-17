package org.erlide.wrangler.refactoring.core.movefunction;

import org.eclipse.ltk.ui.refactoring.RefactoringWizard;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringAction;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringWizard;

public class MoveFunctionAction extends WranglerRefactoringAction {

	@Override
	protected String initRefactoringName() {
		return "Move function";
	}

	@Override
	protected WranglerRefactoring initWranglerRefactoring() {
		return new MoveFunctionRefactoring(parameters);
	}

	@Override
	protected WranglerRefactoringWizard initWranglerRefactoringWizard() {
		return new MoveFunctionWizard(refactoring,
				RefactoringWizard.DIALOG_BASED_USER_INTERFACE);
	}
}
