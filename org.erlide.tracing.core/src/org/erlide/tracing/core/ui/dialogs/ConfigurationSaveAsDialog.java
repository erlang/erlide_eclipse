package org.erlide.tracing.core.ui.dialogs;

import java.util.Set;

import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.swt.widgets.Shell;

/**
 * Dialog window for saving configurations.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class ConfigurationSaveAsDialog extends InputDialog {

    /**
     * Creates dialog window (@see InputDialog).
     * 
     * @param parentShell
     * @param dialogTitle
     * @param dialogMessage
     * @param initialValue
     * @param existingNames
     *            existing configurations
     */
    public ConfigurationSaveAsDialog(final Shell parentShell,
            final String dialogTitle, final String dialogMessage,
            final String initialValue, final Set<String> existingNames) {
        super(parentShell, dialogTitle, dialogMessage, initialValue,
                new Validator(existingNames));
    }

    private static class Validator implements IInputValidator {

        private final Set<String> existingNames;

        public Validator(final Set<String> existingNames) {
            this.existingNames = existingNames;
        }

        @Override
        public String isValid(final String newText) {
            if (newText == null || newText.length() == 0) {
                return "";
            }
            if (existingNames.contains(newText)) {
                return "Configuration with this name already exists";
            }
            return null;
        }
    }
}
