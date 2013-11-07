package org.erlide.ui.wizards;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.erlide.engine.model.builder.BuilderConfig;

public class RebarProjectPreferencesWizardPage extends ProjectPreferencesWizardPage {

    public RebarProjectPreferencesWizardPage(final String pageName,
            final NewProjectData info) {
        super(pageName, info);
        // TODO Auto-generated constructor stub
    }

    @Override
    public void createControl(final Composite parent) {
        // create the composite to hold the widgets
        final Composite composite = new Composite(parent, SWT.NONE);
        setControl(composite);

        checkConfigFile();
        // TODO of course, do something with it :-)
    }

    @Override
    protected String getBuilderDescription() {
        return "Configuration retrieved from " + BuilderConfig.REBAR.getConfigName();
    }

}
