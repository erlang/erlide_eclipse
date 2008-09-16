package org.erlide.wrangler.refactoring.core.foldexpression;

import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringAction;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringWizard;

public class FoldExpressionAction extends WranglerRefactoringAction {

	@Override
	protected String initRefactoringName() {
		return "Fold expresison";
	}

	@Override
	protected WranglerRefactoring initWranglerRefactoring() {
		return new FoldExpressionRefactoring(parameters);
	}

	@Override
	protected WranglerRefactoringWizard initWranglerRefactoringWizard() {
		return new FoldExpressionRefactoringWizard(refactoring, 
				WranglerRefactoringWizard.DIALOG_BASED_USER_INTERFACE);
	}

}
