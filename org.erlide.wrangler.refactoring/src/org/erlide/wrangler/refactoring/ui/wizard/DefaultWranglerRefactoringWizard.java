package org.erlide.wrangler.refactoring.ui.wizard;

import java.util.ArrayList;



import org.eclipse.ltk.core.refactoring.Refactoring;
import org.erlide.wrangler.refactoring.ui.wizardpages.WranglerPage;

public class DefaultWranglerRefactoringWizard extends
		AbstractWranglerRefactoringWizard {

	ArrayList<WranglerPage> pages;

	public DefaultWranglerRefactoringWizard(Refactoring refactoring, int flags,
			ArrayList<WranglerPage> pages) {
		super(refactoring, flags);
		this.pages = pages;
		setWindowTitle(refactoring.getName());
		setDefaultPageTitle(refactoring.getName());
	}

	@Override
	protected void addUserInputPages() {
		for (WranglerPage page : pages) {
			addPage(page);
		}
	}
}
