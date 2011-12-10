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
package org.erlide.wrangler.refactoring.duplicatedcode.ui;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.erlide.wrangler.refactoring.ui.AbstractInputDialog;

/**
 * Input dialog for getting input for Similar expression search refactoring
 * 
 * @author Gyorgy Orosz
 * 
 */
public class SimilarSearchInputDialog extends AbstractInputDialog {

    private Button onlyInFileCheckBoxButton;
    private float simScore;
    private boolean workOnlyInCurrentFile = true;
    private Text simScoreText;

    /**
     * Constructor
     * 
     * @param parentShell
     *            SWT shell
     * @param title
     *            dialog title
     */
    public SimilarSearchInputDialog(final Shell parentShell, final String title) {
        super(parentShell, title);
    }

    @Override
    protected Control createDialogArea(final Composite parent) {
        final Composite composite = (Composite) super.createDialogArea(parent);

        final Label simScoreLabel = new Label(composite, SWT.WRAP);
        simScoreLabel.setText("Similarity score (between 0.1 and 1.0):");
        final GridData simScoreData = new GridData(GridData.GRAB_HORIZONTAL
                | GridData.GRAB_VERTICAL | GridData.HORIZONTAL_ALIGN_FILL
                | GridData.VERTICAL_ALIGN_CENTER);
        simScoreData.widthHint = convertHorizontalDLUsToPixels(IDialogConstants.MINIMUM_MESSAGE_AREA_WIDTH);
        simScoreLabel.setLayoutData(simScoreData);
        simScoreLabel.setFont(parent.getFont());

        simScoreText = new Text(composite, getInputTextStyle());
        simScoreText.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        simScoreText.setText("0.8");
        simScoreText.addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(final ModifyEvent e) {
                validateInput();
            }

        });

        onlyInFileCheckBoxButton = new Button(composite, SWT.CHECK);
        onlyInFileCheckBoxButton
                .setText("Detect similar code snippets in the project");

        onlyInFileCheckBoxButton.setLayoutData(new GridData(
                GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL
                        | GridData.HORIZONTAL_ALIGN_FILL
                        | GridData.VERTICAL_ALIGN_CENTER));
        onlyInFileCheckBoxButton.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetSelected(final SelectionEvent e) {
                validateInput();

            }

            @Override
            public void widgetDefaultSelected(final SelectionEvent e) {
                validateInput();

            }

        });

        errorMessageText = new Text(composite, SWT.READ_ONLY | SWT.WRAP);
        errorMessageText.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        errorMessageText.setBackground(errorMessageText.getDisplay()
                .getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));

        setErrorMessage("");

        applyDialogFont(composite);
        validateInput();
        return composite;
    }

    @Override
    protected void validateInput() {
        String errorMsg = null;
        workOnlyInCurrentFile = !onlyInFileCheckBoxButton.getSelection();
        try {
            simScore = Float.parseFloat(simScoreText.getText());
            setErrorMessage(null);
        } catch (final Exception e) {
            errorMsg = "Please type correct values!";
            setErrorMessage(errorMsg);
        }

    }

    /**
     * Gets the user typed similarity score value
     * 
     * @return similarity score value
     */
    public double getSimScore() {
        return simScore;
    }

    /**
     * Returns the value of the checkbox 'run refactoring in only current
     * module'
     * 
     * @return true if the refactoring should run only in the current module
     */
    public boolean onlyinFile() {
        return workOnlyInCurrentFile;
    }

}
