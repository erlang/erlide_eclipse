package org.erlide.wrangler.refactoring.core.tupleparameters;

import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringAction;
import org.erlide.wrangler.refactoring.ui.WranglerRefactoringWizard;

public class TupleParametersAction extends WranglerRefactoringAction {

	@Override
	protected String initRefactoringName() {
		return "Tuple function parameters";
	}

	@Override
	protected WranglerRefactoring initWranglerRefactoring() {
		return new TupleParametersRefactoring(parameters);
	}

	@Override
	protected WranglerRefactoringWizard initWranglerRefactoringWizard() {
		return new TupleParametersWizard(refactoring,
				WranglerRefactoringWizard.DIALOG_BASED_USER_INTERFACE);
	}

}
