package org.erlide.wrangler.refactoring.ui.validator;

import org.erlide.core.CoreScope;
import org.erlide.core.model.root.ErlModelException;

/**
 * Validator for module names
 * 
 * Check if module exists
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public class ModuleNameValidator extends AtomValidator {

    @Override
    public boolean isValid(final String s) {
        if (!super.isValid(s))
            return false;
        try {
            if (CoreScope.getModel().findModule(s) == null)
                return false;
        } catch (ErlModelException e) {
            return false;
        }
        return true;
    }

}
