package org.erlide.wrangler.refactoring.ui.wizardpages;

import org.eclipse.jface.wizard.IWizardPage;
import org.erlide.wrangler.refactoring.core.internal.ApplyCompositeRefactoring;
import org.erlide.wrangler.refactoring.ui.validator.IValidator;
import org.erlide.wrangler.refactoring.util.CommandData;

public class CompositeRefacInputPage extends UserRefacInputPage {

    public CompositeRefacInputPage(final String name, final String description,
            final String inputErrorMsg, final IValidator validator) {
        super(name, description, inputErrorMsg, validator);
    }

    @Override
    public IWizardPage getNextPage() {
        if (((ApplyCompositeRefactoring) getRefactoring()).fetchNextCommand()) {
            CommandData cmd = ((ApplyCompositeRefactoring) getRefactoring())
                    .getCommand();

            return new UserRefacInputPage(super.getName(), cmd.msg,
                    super.getErrorMessage(), super.validator);
        }
        return super.getNextPage();
    }
}
