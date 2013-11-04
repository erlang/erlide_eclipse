package org.erlide.ui.wizards

import org.eclipse.ui.dialogs.WizardNewProjectCreationPage

class ErlangNewProjectCreationPage extends WizardNewProjectCreationPage {
    val NewProjectData info

    new(String name, NewProjectData info) {
        super(name);
        this.info = info
    }

    override setVisible(boolean visible) {
        super.setVisible(visible)
        if (!visible) {
            info.name = projectName
            info.location = locationPath
        }
    }

}
