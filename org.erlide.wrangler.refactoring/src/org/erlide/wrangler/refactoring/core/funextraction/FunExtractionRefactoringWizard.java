package org.erlide.wrangler.refactoring.core.funextraction;

import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.core.renamefunction.NewFunctionNameInputPage;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringWizard;

public class FunExtractionRefactoringWizard extends WranglerRefactoringWizard {

	public FunExtractionRefactoringWizard(WranglerRefactoring refactoring,
			int flags) {
		super(refactoring, flags);
	}

	@Override
	protected void addUserInputPages() {
		addPage(new NewFunctionNameInputPage("Get new function name"));
	}

	@Override
	protected String initTitle() {
		return "Fun extraction refactoring";
	}

}
