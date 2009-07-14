package org.erlide.wrangler.refactoring.ui.wizardpages;



import org.eclipse.jface.wizard.IWizardPage;
import org.erlide.wrangler.refactoring.core.internal.GeneraliseFunctionRefactoring;
import org.erlide.wrangler.refactoring.ui.validator.IValidator;
import org.erlide.wrangler.refactoring.util.GlobalParameters;

public class HackedSimpleInputPage extends SimpleInputPage {

	public HackedSimpleInputPage(String name, String description,
			String labelText, String inputErrorMsg, IValidator validator) {
		super(name, description, labelText, inputErrorMsg, validator);

	}

	@Override
	public IWizardPage getNextPage() {
		try {
			runUnknownSieEffectCase();
			return super.getNextPage();
		} catch (Exception e) {

			e.printStackTrace();
		}
		return null;
	}

	private void runUnknownSieEffectCase() {
		GeneraliseFunctionRefactoring refac = (GeneraliseFunctionRefactoring) getRefactoring();
		if (refac.isUserInteractionNeeded()) {
			refac.setHasSideEffect(GlobalParameters.showDecidableQuestion(this
					.getShell(),
					"Does the selected expression have side effects?",
					"Unknown side effec"));
		}

	}

	@Override
	protected boolean performFinish() {
		runUnknownSieEffectCase();
		return super.performFinish();
	}

}
