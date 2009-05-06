package org.erlide.wrangler.refactoring.core.renameprocess;

import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringWizard;

public class RenameProcessWizard extends WranglerRefactoringWizard {

	public RenameProcessWizard(WranglerRefactoring refactoring, int flags) {
		super(refactoring, flags);
		// TODO Auto-generated constructor stub
	}

	@Override
	protected void addUserInputPages() {
		addPage(new ProcessNameInputPage("Get new process name"));
	}

	@Override
	protected String initTitle() {
		return "Rename process refactoring";
	}

}
