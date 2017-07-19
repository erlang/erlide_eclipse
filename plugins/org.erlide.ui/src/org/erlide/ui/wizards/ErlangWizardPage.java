package org.erlide.ui.wizards;

import org.eclipse.jface.wizard.WizardPage;

public abstract class ErlangWizardPage extends WizardPage {

    protected ErlangWizardPage(final String pageName) {
        super(pageName);
    }

    @Override
    public void setVisible(final boolean visible) {
        super.setVisible(visible);
        if (visible) {
            onEntry();
        } else {
            onExit();
        }
    }

    protected void onEntry() {
    }

    protected void onExit() {
    }

}
