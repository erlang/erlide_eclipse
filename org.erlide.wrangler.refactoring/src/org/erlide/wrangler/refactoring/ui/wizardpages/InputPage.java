package org.erlide.wrangler.refactoring.ui.wizardpages;

import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.widgets.Shell;

/**
 * Abstratc input page class
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public abstract class InputPage extends WranglerPage {

	/**
	 * @param name
	 *            input page title
	 */
	public InputPage(final String name) {
		super(name);
	}

	@Override
	public IWizardPage getNextPage() {
		controlWorkflow(this.getShell());
		// from UserInPutPageWizard class
		return super.getNextPage();
	}

	@Override
	protected boolean performFinish() {
		controlWorkflow(this.getShell());
		return super.performFinish();
	}

	protected void controlWorkflow(Shell s) {
	}

	abstract protected boolean isInputValid();

}
