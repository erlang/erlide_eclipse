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

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.erlide.wrangler.refactoring.ui.validator.IValidator;
import org.erlide.wrangler.refactoring.ui.validator.VariableNameValidator;

/**
 * Input dialog which accpets a single input data.
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class NewParameterNameInputDialog extends AbstractInputDialog {

    private Text newParameterName;
    private String data;

    /**
     * Constructor
     * 
     * @param parentShell
     *            shell
     * @param title
     *            dialog title
     */
    public NewParameterNameInputDialog(final Shell parentShell,
            final String title) {
        super(parentShell, title);
    }

    /**
     * Get input data
     * 
     * @return input string
     */
    public String getData() {
        return data;
    }

    @Override
    protected Control createDialogArea(final Composite parent) {
        final Composite composite = (Composite) super.createDialogArea(parent);

        final Label newParameterNameLabel = new Label(composite, SWT.WRAP);
        newParameterNameLabel.setText("New parameter name:");
        final GridData minToksData = new GridData(GridData.GRAB_HORIZONTAL
                | GridData.GRAB_VERTICAL | GridData.HORIZONTAL_ALIGN_FILL
                | GridData.VERTICAL_ALIGN_CENTER);
        minToksData.widthHint = convertHorizontalDLUsToPixels(IDialogConstants.MINIMUM_MESSAGE_AREA_WIDTH);
        newParameterNameLabel.setLayoutData(minToksData);
        newParameterNameLabel.setFont(parent.getFont());

        newParameterName = new Text(composite, getInputTextStyle());
        newParameterName.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        newParameterName.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(final ModifyEvent e) {
                data = newParameterName.getText();
                validateInput();
            }
        });

        errorMessageText = new Text(composite, SWT.READ_ONLY | SWT.WRAP);
        errorMessageText.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        errorMessageText.setBackground(errorMessageText.getDisplay()
                .getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));

        setErrorMessage("New parameter name must be a valid variable name!");
        newParameterName.setText("");

        return composite;

    }

    @Override
    protected void validateInput() {
        final IValidator v = new VariableNameValidator();
        if (!v.isValid(newParameterName.getText())) {
            setErrorMessage("New parameter name must be a valid variable name!");
        } else {
            setErrorMessage(null);
        }

    }

}
