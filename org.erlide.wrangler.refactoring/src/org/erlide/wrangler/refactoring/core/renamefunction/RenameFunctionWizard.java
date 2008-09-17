package org.erlide.wrangler.refactoring.core.renamefunction;

import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringWizard;

public class RenameFunctionWizard extends WranglerRefactoringWizard {

	public RenameFunctionWizard(WranglerRefactoring refactoring, int flags) {
		super(refactoring, flags);
	}

	@Override
	protected void addUserInputPages() {
		addPage(new NewFunctionNameInputPage("Get new function name"));
	}

	@Override
	protected String initTitle() {
		return "Rename function refactoring";
	}

}
