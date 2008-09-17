package org.erlide.wrangler.refactoring.core.renamemodule;

import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringWizard;

public class RenameModuleWizard extends WranglerRefactoringWizard {

	public RenameModuleWizard(WranglerRefactoring refactoring, int flags) {
		super(refactoring, flags);
	}

	@Override
	protected void addUserInputPages() {
		addPage(new NewModuleNameInputPage("Get new module name"));

	}

	@Override
	protected String initTitle() {
		return "Rename module refactoring";
	}

}
