package org.erlide.wrangler.refactoring.ui;

import org.eclipse.ltk.ui.refactoring.RefactoringWizard;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;

/**
 * Abstract class for implementing wizards for the Wrangler refactor tool.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public abstract class WranglerRefactoringWizard extends RefactoringWizard {

	/**
	 * Sole construcotor. Initialize the wizard's title.
	 * 
	 * @param refactoring
	 * @param flags
	 */
	public WranglerRefactoringWizard(WranglerRefactoring refactoring, int flags) {
		super(refactoring, flags);
		setWindowTitle(initTitle());
		// setForcePreviousAndNextButtons(true);
		setHelpAvailable(false);
		setDefaultPageTitle(initTitle());
	}

	@Override
	/**
	 * Adds InputPages to the wizard. At least one page is needed.
	 */
	protected abstract void addUserInputPages();

	/**
	 * Returns with the title of the Wizard.
	 * 
	 * @return title of the wizard.
	 */
	protected abstract String initTitle();

}
