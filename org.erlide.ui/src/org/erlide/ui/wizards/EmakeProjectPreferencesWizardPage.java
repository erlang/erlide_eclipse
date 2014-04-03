package org.erlide.ui.wizards;

import org.erlide.engine.model.root.NewProjectData;
import org.erlide.engine.model.root.ProjectConfigType;

public class EmakeProjectPreferencesWizardPage extends ProjectPreferencesWizardPage {

    public EmakeProjectPreferencesWizardPage(final String pageName,
            final NewProjectData info) {
        super(pageName, info);
        configType = ProjectConfigType.EMAKE;
    }

}
