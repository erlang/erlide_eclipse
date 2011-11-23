package org.erlide.wrangler.refactoring.ui.wizardpages;

import org.eclipse.jface.wizard.IWizardPage;
import org.erlide.wrangler.refactoring.core.internal.UserRefactoring;
import org.erlide.wrangler.refactoring.ui.validator.IValidator;

/**
 * Input page for ad hoc custom refactorings, enables to pass the name for the
 * callback module
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public class ModuleInputPage extends SimpleInputPage {

    public ModuleInputPage(final String name, final String description,
            final String labelText, final String inputErrorMsg,
            final IValidator validator) {
        super(name, description, labelText, inputErrorMsg, validator);
    }

    @Override
    protected boolean isInputValid() {
        if (validator.isValid(inputText.getText())) {
            ((UserRefactoring) getRefactoring())
                    .setCallbackModuleName(inputText.getText());
            setErrorMessage(null);
            setPageComplete(true);
            return true;

        } else {
            setPageComplete(false);
            setErrorMessage(inputErrorMsg);
            return false;
        }
    }

    @Override
    public IWizardPage getNextPage() {
        if (!((UserRefactoring) getRefactoring()).fetchParPrompts()) {
            setErrorMessage("Can not load specified callback module");
            setPageComplete(false);
        }
        return super.getNextPage();

    }

}
