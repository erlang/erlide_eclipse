/*******************************************************************************
 * Copyright (c) 2010 György Orosz.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     György Orosz - initial API and implementation
 ******************************************************************************/
package org.erlide.wrangler.refactoring.ui.wizardpages;

import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.widgets.Shell;

/**
 * Abstract input page class
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
        controlWorkflow(getShell());
        // from UserInPutPageWizard class
        return super.getNextPage();
    }

    @Override
    protected boolean performFinish() {
        controlWorkflow(getShell());
        return super.performFinish();
    }

    /**
     * Abstract method which could be use for controlling the workflow.
     * 
     * @param s
     *            shell
     */
    protected void controlWorkflow(final Shell s) {
    }

    abstract protected boolean isInputValid();

}
