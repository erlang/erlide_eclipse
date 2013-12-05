package org.erlide.ui.dialogs;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.erlide.ui.editors.erl.IErlangHelpContextIds;

public class AddInterpretedModulesSelectionDialog extends FilteredModulesSelectionDialog {

    public AddInterpretedModulesSelectionDialog(final Shell shell) {
        super(shell, true, ResourcesPlugin.getWorkspace().getRoot(), IResource.FILE,
                false);
        setTitle("Add module to debug");
        PlatformUI.getWorkbench().getHelpSystem()
                .setHelp(shell, IErlangHelpContextIds.ADD_DEBUG_MODULE_DIALOG);
    }
}
