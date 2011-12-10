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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.erlide.wrangler.refactoring.core.SimpleWranglerRefactoring;
import org.erlide.wrangler.refactoring.ui.validator.IValidator;

/**
 * Input page for wrangler integration which accepts only one parameter
 * 
 * @author Gyorgy Orosz
 * @version %I%, %G%
 */
public class SimpleInputPage extends InputPage {
    protected String defaultInput = "";
    protected String labelText;

    protected String inputErrorMsg;

    IValidator validator;

    protected Label inputLabel;

    protected Text inputText;

    protected Composite composite;

    /**
     * Constructor
     * 
     * @param name
     *            Refactoring name (title)
     * @param description
     *            description
     * @param labelText
     *            input label's text
     * @param inputErrorMsg
     *            error message in case of wrong input
     * @param validator
     *            validator object
     */
    public SimpleInputPage(final String name, final String description,
            final String labelText, final String inputErrorMsg,
            final IValidator validator) {
        super(name);
        setDescription(description);
        this.inputErrorMsg = inputErrorMsg;
        this.labelText = labelText;
        this.validator = validator;
        setPageComplete(false);

    }

    @Override
    public void createControl(final Composite parent) {
        composite = new Composite(parent, SWT.NONE);

        inputLabel = new Label(composite, SWT.LEFT);
        inputLabel.setText(labelText);
        GridData gridData = new GridData();
        gridData.horizontalAlignment = GridData.FILL;
        gridData.horizontalSpan = 2;
        inputLabel.setLayoutData(gridData);

        inputText = new Text(composite, SWT.NONE);
        gridData = new GridData();
        gridData.horizontalAlignment = GridData.FILL;
        gridData.horizontalSpan = 2;
        gridData.grabExcessHorizontalSpace = true;
        inputText.setLayoutData(gridData);

        final GridLayout layout = new GridLayout();
        layout.numColumns = 2;
        composite.setLayout(layout);

        setControl(composite);

        inputText.addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(final ModifyEvent e) {
                isInputValid();
            }

        });

        inputText.setText(defaultInput);
        inputText.setFocus();
        inputText.setSelection(0, defaultInput.length());
        ((SimpleWranglerRefactoring) getRefactoring()).setUserInput(inputText
                .getText());
    }

    @Override
    protected boolean isInputValid() {
        if (validator.isValid(inputText.getText())) {
            ((SimpleWranglerRefactoring) getRefactoring())
                    .setUserInput(inputText.getText());
            setErrorMessage(null);
            setPageComplete(true);
            return true;
        } else {
            setPageComplete(false);
            setErrorMessage(inputErrorMsg);
            return false;
        }
    }

    /**
     * Sets the input text for the page
     * 
     * @param input
     *            default input text
     */
    public void setInput(final String input) {
        defaultInput = input;
    }
}
