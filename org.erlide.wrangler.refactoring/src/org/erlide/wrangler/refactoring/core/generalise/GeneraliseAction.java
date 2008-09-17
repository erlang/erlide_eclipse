package org.erlide.wrangler.refactoring.core.generalise;

import org.eclipse.ltk.ui.refactoring.RefactoringWizard;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringAction;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringWizard;

public class GeneraliseAction extends WranglerRefactoringAction {

	@Override
	protected String initRefactoringName() {
		return "Generalise function";
	}

	@Override
	protected WranglerRefactoring initWranglerRefactoring() {
		return new GeneraliseRefactoring(parameters);
	}

	@Override
	protected WranglerRefactoringWizard initWranglerRefactoringWizard() {
		return new GeneraliseWizard(refactoring,
				RefactoringWizard.DIALOG_BASED_USER_INTERFACE);
	}
}
