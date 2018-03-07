package org.erlide.ui.editors.scratchpad;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;

public class NewErlangScratchPadWizardPage extends WizardNewFileCreationPage {

    public NewErlangScratchPadWizardPage(final IStructuredSelection selection) {
        super("NewErlangScratchPadWizardPage", selection);
        setTitle("Erlang Scratch Pad");
        setDescription("This wizard creates a new erlang scratch pad.");
    }

}
