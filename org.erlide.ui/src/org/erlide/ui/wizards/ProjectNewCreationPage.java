package org.erlide.ui.wizards;

import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.erlide.ui.wizards.NewErlangProject.ProjectInfo;

public class ProjectNewCreationPage extends WizardNewProjectCreationPage {

    private final ProjectInfo info;

    public ProjectNewCreationPage(final String name, final ProjectInfo info) {
        super(name);
        this.info = info;
    }
}
