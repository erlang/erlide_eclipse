package org.erlide.wrangler.refactoring.ui.wizardpages;

import org.eclipse.jface.wizard.IWizardPage;
import org.erlide.wrangler.refactoring.core.internal.ApplyCompositeRefactoring;
import org.erlide.wrangler.refactoring.ui.validator.IValidator;

public class MainCompositeRefacInputPage extends CompositeRefacInputPage {

    public MainCompositeRefacInputPage(final String name,
            final String description, final String inputErrorMsg,
            final IValidator validator) {
        super(name, description, inputErrorMsg, validator);
    }

    @Override
    public IWizardPage getNextPage() {
        if (!((ApplyCompositeRefactoring) getRefactoring())
                .startCompositeRefactoring()) {
            setErrorMessage("Errors while starting refactoring");
            setPageComplete(false);
            return this; // TODO check if status can be changed
        }

        return super.getNextPage();
    }

}
