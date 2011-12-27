package org.erlide.wrangler.refactoring.ui.validator;

import org.erlide.core.ErlangCore;
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
        if (!super.isValid(s)) {
            return false;
        }
        try {
            if (ErlangCore.getModel().findModule(s) == null) {
                return false;
            }
        } catch (final ErlModelException e) {
            return false;
        }
        return true;
    }

}
