package org.erlide.wrangler.refactoring.core.renamevariable;

import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringWizard;

public class RenameVariableWizard extends WranglerRefactoringWizard {

	public RenameVariableWizard(WranglerRefactoring refactoring, int flags) {
		super(refactoring, flags);
	}

	@Override
	protected void addUserInputPages() {
		addPage(new NewVariableNameInputPage("Get new variable name"));
	}

	@Override
	protected String initTitle() {
		return "Rename variable refactoring";
	}

}
