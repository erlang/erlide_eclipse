package org.erlide.wrangler.refactoring.core.funextraction;

import org.eclipse.ltk.ui.refactoring.RefactoringWizard;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringAction;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringWizard;

public class FunExtractionAction extends WranglerRefactoringAction {

	@Override
	protected String initRefactoringName() {
		return "Fun extraction refactoring";
	}

	@Override
	protected WranglerRefactoring initWranglerRefactoring() {
		return new FunExtractionRefactoring(parameters);
	}

	@Override
	protected WranglerRefactoringWizard initWranglerRefactoringWizard() {
		return new FunExtractionRefactoringWizard(refactoring,
				RefactoringWizard.DIALOG_BASED_USER_INTERFACE);
	}

}
