package org.erlide.tracing.core.ui.dialogs;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;

/**
 * Dialog for selecting configuration for loading.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class SelectConfigurationDialog extends ElementListSelectionDialog {

    public SelectConfigurationDialog(final Shell parent,
            final ILabelProvider renderer) {
        super(parent, renderer);
        setTitle("Select configuration");
        setEmptyListMessage("no configuration");
        setMultipleSelection(false);
        setHelpAvailable(false);
        setMessage("Select a configuration (* = any string, ? = any char):");
    }
}
