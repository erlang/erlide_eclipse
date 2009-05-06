package org.erlide.wrangler.refactoring.core.functiontoprocess;

import org.eclipse.ltk.ui.refactoring.RefactoringWizard;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringAction;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringWizard;

public class FunctionToProcessAction extends WranglerRefactoringAction {

	@Override
	protected String initRefactoringName() {
		return "Function to process";
	}

	@Override
	protected WranglerRefactoring initWranglerRefactoring() {
		return new FunctionToProcessRefactoring(parameters);
	}

	@Override
	protected WranglerRefactoringWizard initWranglerRefactoringWizard() {
		return new FunctionToProcessWizard(refactoring,
				RefactoringWizard.DIALOG_BASED_USER_INTERFACE);
	}

}
