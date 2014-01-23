package org.erlide.ui.wizards;

import org.erlide.engine.model.builder.BuilderConfigType;
import org.erlide.engine.model.root.NewProjectData;

public class RebarProjectPreferencesWizardPage extends ProjectPreferencesWizardPage {

    public RebarProjectPreferencesWizardPage(final String pageName,
            final NewProjectData info) {
        super(pageName, info);
        configType = BuilderConfigType.REBAR;
    }

}
