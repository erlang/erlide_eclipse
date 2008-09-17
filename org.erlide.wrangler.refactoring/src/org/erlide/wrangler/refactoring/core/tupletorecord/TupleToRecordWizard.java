package org.erlide.wrangler.refactoring.core.tupletorecord;

import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringWizard;

public class TupleToRecordWizard extends WranglerRefactoringWizard {

	public TupleToRecordWizard(WranglerRefactoring refactoring, int flags) {
		super(refactoring, flags);
	}

	@Override
	protected void addUserInputPages() {
		addPage(new NewParametersNameInputPage("Get new parameters name"));
	}

	@Override
	protected String initTitle() {
		return "Tuple to record refactoring";
	}

}
