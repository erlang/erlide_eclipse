package org.erlide.ui.wizards;

import org.erlide.engine.model.builder.BuilderConfigType;

public class RebarProjectPreferencesWizardPage extends ProjectPreferencesWizardPage {

    public RebarProjectPreferencesWizardPage(final String pageName,
            final NewProjectData info) {
        super(pageName, info);
        configType = BuilderConfigType.REBAR;
    }

}
