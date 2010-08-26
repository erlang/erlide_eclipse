package org.ttb.integration.ui.dialogs;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.ttb.integration.mvc.model.ConfigurationManager;

/**
 * Dialog for selecting tracing configuration.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class SelectTracingConfigurationDialog extends ElementListSelectionDialog {

    public SelectTracingConfigurationDialog(Shell parent, ILabelProvider renderer) {
        super(parent, renderer);
        setTitle("Select configuration");
        setEmptyListMessage("no configuration");
        setMultipleSelection(false);
        setHelpAvailable(false);
        setMessage("Select a configuration (* = any string, ? = any char):");
        setElements(ConfigurationManager.getTracePatternsConfigurations());
    }
}
