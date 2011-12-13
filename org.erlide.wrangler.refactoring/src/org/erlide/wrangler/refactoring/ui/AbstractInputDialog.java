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
package org.erlide.wrangler.refactoring.ui;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * Abstract class for creating input dialogs outside from a Wizard.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public abstract class AbstractInputDialog extends Dialog {

    protected Text errorMessageText;
    protected final String title;
    protected boolean isFinished = false;
    protected Button okButton;

    /**
     * Constructor
     * 
     * @param parentShell
     *            shell
     * @param title
     *            dialog title
     */
    public AbstractInputDialog(final Shell parentShell, final String title) {
        super(parentShell);
        this.title = title;
    }

    abstract protected void validateInput();

    /**
     * Returns true if the dialog is finsihed normally
     * 
     * @return false if closed unexpectedly
     */
    public boolean isFinished() {
        return isFinished;
    }

    /**
     * Set error message on the dialog
     * 
     * @param errorMessage
     *            error message
     */
    public void setErrorMessage(final String errorMessage) {
        if (errorMessageText != null && !errorMessageText.isDisposed()) {
            errorMessageText.setText(errorMessage == null ? " \n "
                    : errorMessage);

            final boolean hasError = errorMessage != null
                    && StringConverter.removeWhiteSpaces(errorMessage).length() > 0;
            errorMessageText.setEnabled(hasError);
            errorMessageText.setVisible(hasError);
            errorMessageText.getParent().update();

            final Control button = getButton(IDialogConstants.OK_ID);
            if (button != null) {
                button.setEnabled(errorMessage == null);
            }
        }
    }

    @Override
    protected void configureShell(final Shell shell) {
        super.configureShell(shell);
        if (title != null) {
            shell.setText(title);
        }
    }

    protected int getInputTextStyle() {
        return SWT.SINGLE | SWT.BORDER;
    }

    @Override
    protected void createButtonsForButtonBar(final Composite parent) {
        okButton = createButton(parent, IDialogConstants.OK_ID,
                IDialogConstants.OK_LABEL, true);
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);

        okButton.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(final SelectionEvent e) {
            }

            @Override
            public void widgetSelected(final SelectionEvent e) {
                isFinished = true;
            }

        });
    }

}
