package org.erlide.wrangler.refactoring.ui.wizardpages;

import org.eclipse.swt.widgets.Shell;
import org.erlide.wrangler.refactoring.core.CostumWorkflowRefactoring;
import org.erlide.wrangler.refactoring.core.RefactoringWorkflowController;
import org.erlide.wrangler.refactoring.ui.validator.IValidator;

public class CostumworkFlowInputPage extends SimpleInputPage {

	protected CostumWorkflowRefactoring costumWrokflowRefactoring;
	protected RefactoringWorkflowController workflowController;

	public CostumworkFlowInputPage(String name, String description,
			String labelText, String inputErrorMsg, IValidator validator) {
		super(name, description, labelText, inputErrorMsg, validator);
	}

	protected void controlWorkflow(Shell s) {
		setCostumRefactoring();
		workflowController = costumWrokflowRefactoring.getWorkflowController(s);
		workflowController.doRefactoring();
	}

	/*
	 * @Override public boolean isLastUserInputPage() { setCostumRefactoring();
	 * if (workflowController.controlInputPagesOrder()) return
	 * workflowController.isLastPage(); else return super.isLastUserInputPage();
	 * }
	 */

	protected void setCostumRefactoring() {
		if (costumWrokflowRefactoring == null)
			costumWrokflowRefactoring = (CostumWorkflowRefactoring) getRefactoring();
	}

	@Override
	public boolean canFlipToNextPage() {
		return isPageComplete();
	}
}
