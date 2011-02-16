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
package org.erlide.wrangler.refactoring.core;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;

/**
 * Class is for controlling costum behaviour of a Wrangler refactoring
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public abstract class RefactoringWorkflowController {
    Shell shell;

    /**
     * Constructor
     * 
     * @param shell
     *            shell
     */
    public RefactoringWorkflowController(final Shell shell) {
        this.shell = shell;
    }

    /**
     * Call the RPC.
     */
    public abstract void doRefactoring();

    /**
     * Abort the refactoring.
     */
    public void stop() {
        shell.close();
    }

    /**
     * Ask a question from the user in an input dialog.
     * 
     * @param title
     *            dialog title
     * @param message
     *            queestion
     * @return true if the answer is yes, else false
     */
    public boolean ask(final String title, final String message) {
        return MessageDialog.openQuestion(shell, title, message);
    }

}
