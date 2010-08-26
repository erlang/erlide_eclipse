package org.ttb.integration.ui.dialogs;

import java.util.Arrays;
import java.util.HashSet;

import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.swt.widgets.Shell;
import org.ttb.integration.mvc.model.ConfigurationManager;

/**
 * Dialog window for saving tracing configurations.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TracingConfigurationSaveAsDialog extends InputDialog {

    /**
     * @see InputDialog
     */
    public TracingConfigurationSaveAsDialog(Shell parentShell, String dialogTitle, String dialogMessage, String initialValue) {
        super(parentShell, dialogTitle, dialogMessage, initialValue, new Validator());
    }

    private static class Validator implements IInputValidator {

        private final HashSet<String> existingNames;

        public Validator() {
            String[] configurations = ConfigurationManager.getTracePatternsConfigurations();
            existingNames = new HashSet<String>(Arrays.asList(configurations));
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
