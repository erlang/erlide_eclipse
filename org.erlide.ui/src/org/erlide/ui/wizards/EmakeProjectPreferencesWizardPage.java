package org.erlide.ui.wizards;

import org.eclipse.swt.widgets.Control;
import org.erlide.engine.model.builder.BuilderConfig;

public class EmakeProjectPreferencesWizardPage extends ProjectPreferencesWizardPage {

    public EmakeProjectPreferencesWizardPage(final String pageName,
            final NewProjectData info) {
        super(pageName, info);
        // TODO Auto-generated constructor stub
    }

    @Override
    protected String getBuilderDescription() {
        return "Configuration retrieved from " + BuilderConfig.EMAKE.getConfigName();
    }

    @Override
    public Control getControl() {
        return super.getControl();
    }
}
