package org.erlide.wrangler.refactoring.ui.validator;

/**
 * Validates a string which is a double values and is between 0.1 and 1.0
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class NormalDoulbeValidator implements IValidator {

    @Override
    public boolean isValid(final String text) {
        try {
            final Double val = Double.parseDouble(text);
            return val <= 1 && val >= 0.1;
        } catch (final Exception e) {
            return false;
        }
    }
}
