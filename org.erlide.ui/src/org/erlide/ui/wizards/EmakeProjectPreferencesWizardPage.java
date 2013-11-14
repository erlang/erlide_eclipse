package org.erlide.ui.wizards;

import org.erlide.engine.model.builder.BuilderConfig;

public class EmakeProjectPreferencesWizardPage extends ProjectPreferencesWizardPage {

    public EmakeProjectPreferencesWizardPage(final String pageName,
            final NewProjectData info) {
        super(pageName, info);
    }

    @Override
    protected String getBuilderDescription() {
        return "Configuration retrieved from " + BuilderConfig.EMAKE.getConfigName();
    }

}
