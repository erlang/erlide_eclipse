package org.erlide.wrangler.refactoring.core.functiontoprocess;

import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.core.renameprocess.ProcessNameInputPage;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringWizard;

public class FunctionToProcessWizard extends WranglerRefactoringWizard {

	public FunctionToProcessWizard(WranglerRefactoring refactoring, int flags) {
		super(refactoring, flags);
	}

	@Override
	protected void addUserInputPages() {
		addPage(new ProcessNameInputPage("Get new process name"));

	}

	@Override
	protected String initTitle() {
		return "Function to process";
	}

}
