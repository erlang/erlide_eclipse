package org.erlide.wrangler.refactoring.ui.wizardpages;

import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.ltk.core.refactoring.Refactoring;
import org.erlide.wrangler.refactoring.core.CostumWorkflowRefactoring;

public abstract class InputPage extends WranglerPage {

	public InputPage(String name) {
		super(name);
	}

	@Override
	public IWizardPage getNextPage() {
		controlWorkflow();
		return super.getNextPage();
	}

	@Override
	protected boolean performFinish() {
		controlWorkflow();
		return super.performFinish();
	}

	protected void controlWorkflow() {
		Refactoring ref = getRefactoring();
		if (ref instanceof CostumWorkflowRefactoring) {
			CostumWorkflowRefactoring cref = (CostumWorkflowRefactoring) ref;
			cref.getWorkflowController(this.getShell()).doRefactoring();
		}
	}
}
