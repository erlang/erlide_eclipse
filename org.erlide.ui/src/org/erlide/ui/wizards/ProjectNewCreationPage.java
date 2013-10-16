package org.erlide.ui.wizards;

import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.erlide.engine.model.root.IErlangProjectProperties;

public class ProjectNewCreationPage extends WizardNewProjectCreationPage {

    private final IErlangProjectProperties info;

    public ProjectNewCreationPage(final String name, final IErlangProjectProperties info) {
        super(name);
        this.info = info;
    }
}
