package org.erlide.wrangler.refactoring.ui.wizardpages;

import org.erlide.wrangler.refactoring.core.internal.ApplyAdhocRefactoring;
import org.erlide.wrangler.refactoring.ui.validator.IValidator;

/**
 * Input page for ad hoc custom refactorings, enables to pass the name for the
 * callback module
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public class ModuleInputPage extends SimpleInputPage {

    /*
     * private String inputErrorMsg; private String labelText; private Label
     * inputLabel; private Text inputText; IValidator validator;
     * 
     * public ModuleInputPage(final String name, final String description, final
     * String labelText, final String inputErrorMsg, final IValidator validator)
     * { super(name); setDescription(description); this.inputErrorMsg =
     * inputErrorMsg; this.labelText = labelText; this.validator = validator;
     * setPageComplete(false);
     * 
     * }
     */

    public ModuleInputPage(final String name, final String description,
            final String labelText, final String inputErrorMsg,
            final IValidator validator) {
        super(name, description, labelText, inputErrorMsg, validator);
    }

    @Override
    protected boolean isInputValid() {
        if (validator.isValid(inputText.getText())) {
            ((ApplyAdhocRefactoring) getRefactoring())
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

}
