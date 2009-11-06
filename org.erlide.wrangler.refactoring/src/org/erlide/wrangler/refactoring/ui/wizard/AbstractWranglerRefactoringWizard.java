package org.erlide.wrangler.refactoring.ui.wizard;

import org.eclipse.ltk.core.refactoring.Refactoring;
import org.eclipse.ltk.ui.refactoring.RefactoringWizard;

public abstract class AbstractWranglerRefactoringWizard extends
		RefactoringWizard {

	public AbstractWranglerRefactoringWizard(Refactoring refactoring, int flags) {
		super(refactoring, flags);
	}

}
