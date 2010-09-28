package org.ttb.integration.ui.dialogs;

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
    public ConfigurationSaveAsDialog(Shell parentShell, String dialogTitle, String dialogMessage, String initialValue, Set<String> existingNames) {
        super(parentShell, dialogTitle, dialogMessage, initialValue, new Validator(existingNames));
    }

    private static class Validator implements IInputValidator {

        private final Set<String> existingNames;

        public Validator(Set<String> existingNames) {
            this.existingNames = existingNames;
        }

        public String isValid(String newText) {
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
