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
 * Input dialog for getting necessary input for duplicated code detection
 * refactoring
 * 
 * @author Gyorgy Orosz
 * 
 */
public class DuplicateCodeDetectionInputDialog extends AbstractInputDialog {

    private int minToks;

    private int minClones;

    private boolean workOnlyInCurrentFile;

    private Text minToksText;

    private Text minClonesText;

    private Button onlyInFileCheckBoxButton;

    /**
     * Constructor
     * 
     * @param parentShell
     *            SWT shell
     * @param dialogTitle
     *            dialog title
     */
    public DuplicateCodeDetectionInputDialog(final Shell parentShell,
            final String dialogTitle) {
        super(parentShell, dialogTitle);
    }

    /**
     * Gets the value of minimal tokens
     * 
     * @return number of minimal tokens
     */
    public int getMinToks() {
        return minToks;
    }

    /**
     * Gets the value of minimal clone numbers
     * 
     * @return minimal clones number
     */
    public int getMinClones() {
        return minClones;
    }

    /**
     * Gets the value of the 'run only in the current module' checkbox
     * 
     * @return true, if the refactoring will be run in only the current module
     */
    public boolean onlyInFile() {
        return workOnlyInCurrentFile;
    }

    /*
     * @Override protected void buttonPressed(int buttonId) {
     * super.buttonPressed(buttonId); }
     */

    @Override
    protected void createButtonsForButtonBar(final Composite parent) {
        super.createButtonsForButtonBar(parent);

        minToksText.setFocus();
        minToksText.setText("");
        minToksText.selectAll();

    }

    @Override
    protected Control createDialogArea(final Composite parent) {
        // create composite
        final Composite composite = (Composite) super.createDialogArea(parent);
        // create message

        final Label minTokslabel = new Label(composite, SWT.WRAP);
        minTokslabel
                .setText("Minimal numbers of tokens a code clone should have:");
        final GridData minToksData = new GridData(GridData.GRAB_HORIZONTAL
                | GridData.GRAB_VERTICAL | GridData.HORIZONTAL_ALIGN_FILL
                | GridData.VERTICAL_ALIGN_CENTER);
        minToksData.widthHint = convertHorizontalDLUsToPixels(IDialogConstants.MINIMUM_MESSAGE_AREA_WIDTH);
        minTokslabel.setLayoutData(minToksData);
        minTokslabel.setFont(parent.getFont());

        minToksText = new Text(composite, getInputTextStyle());
        minToksText.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        minToksText.setText("20");
        minToksText.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(final ModifyEvent e) {
                validateInput();
            }
        });

        final Label minClonesLabel = new Label(composite, SWT.WRAP);
        minClonesLabel.setText("Minimum number of appearance times:");
        final GridData minClonesData = new GridData( // GridData.GRAB_HORIZONTAL
                                                     // |
                GridData.GRAB_VERTICAL // | GridData.HORIZONTAL_ALIGN_FILL
                        | GridData.VERTICAL_ALIGN_CENTER);
        minClonesData.widthHint = convertHorizontalDLUsToPixels(IDialogConstants.MINIMUM_MESSAGE_AREA_WIDTH);
        minClonesLabel.setLayoutData(minClonesData);
        minClonesLabel.setFont(parent.getFont());
        minClonesText = new Text(composite, getInputTextStyle());
        minClonesText.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        minClonesText.setText("2");
        minClonesText.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(final ModifyEvent e) {
                validateInput();
            }
        });

        onlyInFileCheckBoxButton = new Button(composite, SWT.CHECK);
        onlyInFileCheckBoxButton.setText("Detect duplicates in the project");
        onlyInFileCheckBoxButton.setLayoutData(new GridData(
                GridData.GRAB_HORIZONTAL | GridData.HORIZONTAL_ALIGN_FILL));
        onlyInFileCheckBoxButton.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(final SelectionEvent e) {
            }

            @Override
            public void widgetSelected(final SelectionEvent e) {
                validateInput();

            }

        });
        errorMessageText = new Text(composite, SWT.READ_ONLY | SWT.WRAP);
        errorMessageText.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        errorMessageText.setBackground(errorMessageText.getDisplay()
                .getSystemColor(SWT.COLOR_WIDGET_BACKGROUND));

        setErrorMessage("Please use only integers!");

        applyDialogFont(composite);
        return composite;
    }

    @Override
    protected void validateInput() {
        String errorMsg = null;
        try {
            workOnlyInCurrentFile = !onlyInFileCheckBoxButton.getSelection();
            minToks = Integer.parseInt(minToksText.getText());
            minClones = Integer.parseInt(minClonesText.getText());
            setErrorMessage(null);
        } catch (final Exception e) {
            errorMsg = "Minimal number of clones and tokens should be integers.";
            setErrorMessage(errorMsg);
        }

    }
}
