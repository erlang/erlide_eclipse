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
package org.erlide.wrangler.refactoring.codeinspection.ui;

import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

/**
 * Simple input dialog with a checkbox
 * 
 * @author Gyorgy Orosz
 * 
 */
public class InputDialogWithCheckbox extends InputDialog {
    protected String checkBoxText;
    protected Button checkBox;
    protected boolean isCheckBoxSelected;

    /**
     * Constructor
     * 
     * @param parentShell
     *            parent shell
     * @param dialogTitle
     *            dialog title
     * @param dialogMessage
     *            dialog message
     * @param checkBoxText
     *            checkbox text
     * @param initialValue
     *            initial value
     * @param validator
     *            validator object
     */
    public InputDialogWithCheckbox(final Shell parentShell,
            final String dialogTitle, final String dialogMessage,
            final String checkBoxText, final String initialValue,
            final IInputValidator validator) {
        super(parentShell, dialogTitle, dialogMessage, initialValue, validator);
        this.checkBoxText = checkBoxText;

    }

    @Override
    protected Control createDialogArea(final Composite parent) {
        final Composite c = (Composite) super.createDialogArea(parent);
        checkBox = new Button(c, SWT.CHECK);
        checkBox.setText(checkBoxText);

        checkBox.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.GRAB_VERTICAL | GridData.HORIZONTAL_ALIGN_FILL
                | GridData.VERTICAL_ALIGN_CENTER));
        checkBox.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(final SelectionEvent e) {
                isCheckBoxSelected = checkBox.getSelection();

            }

            @Override
            public void widgetDefaultSelected(final SelectionEvent e) {
                widgetSelected(e);
            }
        });

        return c;
    }

    /**
     * Returns the checkbox last value.
     * 
     * @return true if the checkbox was selected
     */
    public boolean isCheckBoxChecked() {
        return isCheckBoxSelected;
    }
}
