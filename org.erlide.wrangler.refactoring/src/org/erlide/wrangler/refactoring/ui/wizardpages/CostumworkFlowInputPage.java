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

import org.eclipse.swt.widgets.Shell;
import org.erlide.wrangler.refactoring.core.CostumWorkflowRefactoring;
import org.erlide.wrangler.refactoring.core.RefactoringWorkflowController;
import org.erlide.wrangler.refactoring.ui.validator.IValidator;

/**
 * An Input page which hacks the standard workflow and allows to run costum
 * commands after pressing next or finish.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class CostumworkFlowInputPage extends SimpleInputPage {

    protected CostumWorkflowRefactoring costumWrokflowRefactoring;
    protected RefactoringWorkflowController workflowController;

    /**
     * Default constructor
     * 
     * @param name
     *            Input page title
     * @param description
     *            Input page description
     * @param labelText
     *            input data label
     * @param inputErrorMsg
     *            error message in case of the user mistyped sg
     * @param validator
     *            validator object
     */
    public CostumworkFlowInputPage(final String name, final String description,
            final String labelText, final String inputErrorMsg,
            final IValidator validator) {
        super(name, description, labelText, inputErrorMsg, validator);
    }

    @Override
    protected void controlWorkflow(final Shell s) {
        setCostumRefactoring();
        workflowController = costumWrokflowRefactoring.getWorkflowController(s);
        workflowController.doRefactoring();
    }

    /*
     * @Override public boolean isLastUserInputPage() { setCostumRefactoring();
     * if (workflowController.controlInputPagesOrder()) return
     * workflowController.isLastPage(); else return super.isLastUserInputPage();
     * }
     */

    protected void setCostumRefactoring() {
        if (costumWrokflowRefactoring == null) {
            costumWrokflowRefactoring = (CostumWorkflowRefactoring) getRefactoring();
        }
    }

    @Override
    public boolean canFlipToNextPage() {
        return isPageComplete();
    }
}
