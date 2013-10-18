package org.erlide.ui.wizards;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.erlide.engine.model.root.IErlangProjectProperties;

public class MakeProjectPreferencesWizardPage extends ProjectPreferencesWizardPage {

    public MakeProjectPreferencesWizardPage(final String pageName,
            final IErlangProjectProperties info) {
        super(pageName, info);
        // TODO Auto-generated constructor stub
    }

    @Override
    public void createControl(final Composite parent) {
        // create the composite to hold the widgets
        final Composite composite = new Composite(parent, SWT.NONE);
        setControl(composite);
        // TODO Auto-generated method stub
        setDescription("MAKE");

    }

}
