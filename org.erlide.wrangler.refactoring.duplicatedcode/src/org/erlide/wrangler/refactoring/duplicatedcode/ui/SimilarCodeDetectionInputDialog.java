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
 * Input dialog for getting input for the similar code detection refactorings
 * 
 * @author Gyorgy Orosz
 * 
 */
public class SimilarCodeDetectionInputDialog extends AbstractInputDialog {

    private Button onlyInFileCheckBoxButton;
    private Text minLenText;
    private Text minFreqText;
    private Text minToksText;
    private Text maxNewVarsText;

    private int minLen;
    private int minFreq;
    private int minToks;
    private int maxNewVars;
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
    public SimilarCodeDetectionInputDialog(final Shell parentShell,
            final String title) {
        super(parentShell, title);
    }

    @Override
    protected void validateInput() {
        String errorMsg = null;
        workOnlyInCurrentFile = !onlyInFileCheckBoxButton.getSelection();
        try {
            simScore = Float.parseFloat(simScoreText.getText());
            minLen = Integer.parseInt(minLenText.getText());
            minFreq = Integer.parseInt(minFreqText.getText());
            minToks = Integer.parseInt(minToksText.getText());
            maxNewVars = Integer.parseInt(maxNewVarsText.getText());
            setErrorMessage(null);
        } catch (final Exception e) {
            errorMsg = "Please type correct values!";
            setErrorMessage(errorMsg);
        }

    }

    @Override
    protected Control createDialogArea(final Composite parent) {
        final Composite composite = (Composite) super.createDialogArea(parent);

        final Label minLenlabel = new Label(composite, SWT.WRAP);
        minLenlabel.setText("Minimum lenght of an expression sequence:");
        final GridData minLenData = new GridData(GridData.GRAB_HORIZONTAL
                | GridData.GRAB_VERTICAL | GridData.HORIZONTAL_ALIGN_FILL
                | GridData.VERTICAL_ALIGN_CENTER);
        minLenData.widthHint = convertHorizontalDLUsToPixels(IDialogConstants.MINIMUM_MESSAGE_AREA_WIDTH);
        minLenlabel.setLayoutData(minLenData);
        minLenlabel.setFont(parent.getFont());

        minLenText = new Text(composite, getInputTextStyle());
        minLenText.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        minLenText.setText("5");
        minLenText.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(final ModifyEvent e) {
                validateInput();
            }
        });

        final Label minTokslabel = new Label(composite, SWT.WRAP);
        minTokslabel.setText("Minimum number of tokens a clone should have:");
        final GridData minToksData = new GridData(GridData.GRAB_HORIZONTAL
                | GridData.GRAB_VERTICAL | GridData.HORIZONTAL_ALIGN_FILL
                | GridData.VERTICAL_ALIGN_CENTER);
        minToksData.widthHint = convertHorizontalDLUsToPixels(IDialogConstants.MINIMUM_MESSAGE_AREA_WIDTH);
        minTokslabel.setLayoutData(minToksData);
        minTokslabel.setFont(parent.getFont());

        minToksText = new Text(composite, getInputTextStyle());
        minToksText.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        minToksText.setText("40");
        minToksText.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(final ModifyEvent e) {
                validateInput();
            }
        });

        final Label minFreqLabel = new Label(composite, SWT.WRAP);
        minFreqLabel.setText("Minimum number appearance times:");
        final GridData minFreqData = new GridData(GridData.GRAB_HORIZONTAL
                | GridData.GRAB_VERTICAL | GridData.HORIZONTAL_ALIGN_FILL
                | GridData.VERTICAL_ALIGN_CENTER);
        minFreqData.widthHint = convertHorizontalDLUsToPixels(IDialogConstants.MINIMUM_MESSAGE_AREA_WIDTH);
        minFreqLabel.setLayoutData(minFreqData);
        minFreqLabel.setFont(parent.getFont());

        minFreqText = new Text(composite, getInputTextStyle());
        minFreqText.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        minFreqText.setText("2");
        minFreqText.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(final ModifyEvent e) {
                validateInput();
            }
        });

        final Label maxNewVarsLabel = new Label(composite, SWT.WRAP);
        maxNewVarsLabel
                .setText("Maximum number of new parameters of the least-general abstraction:");
        final GridData maxNewVarsData = new GridData(GridData.GRAB_HORIZONTAL
                | GridData.GRAB_VERTICAL | GridData.HORIZONTAL_ALIGN_FILL
                | GridData.VERTICAL_ALIGN_CENTER);
        maxNewVarsData.widthHint = convertHorizontalDLUsToPixels(IDialogConstants.MINIMUM_MESSAGE_AREA_WIDTH);
        maxNewVarsLabel.setLayoutData(maxNewVarsData);
        maxNewVarsLabel.setFont(parent.getFont());

        maxNewVarsText = new Text(composite, getInputTextStyle());
        maxNewVarsText.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
                | GridData.HORIZONTAL_ALIGN_FILL));
        maxNewVarsText.setText("4");
        maxNewVarsText.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(final ModifyEvent e) {
                validateInput();
            }
        });

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

    /*
     * protected Spinner createValueSpinner(Composite parent, int min, int max,
     * int digits, int value) { Spinner ret = new Spinner(parent, SWT.BORDER);
     * ret.setMinimum(min); ret.setMaximum(max); ret.setDigits(digits);
     * ret.setSelection(value);
     * 
     * GridData gd = new GridData(); gd.verticalIndent = 2;
     * gd.horizontalAlignment = SWT.RIGHT; gd.grabExcessHorizontalSpace = true;
     * ret.setLayoutData(gd);
     * 
     * return ret; }
     */

    /**
     * Returns the user typed similarity score.
     * 
     * @return similarity scores
     */
    public double getSimScore() {
        return simScore;
    }

    /**
     * Returns the user typed minimal length
     * 
     * @return minimal length
     */
    public int getMinLen() {
        return minLen;
    }

    /**
     * 
     * @return minimal number of tokens
     */
    public int getMinToks() {
        return minToks;
    }

    /**
     * 
     * @return maximal number of new variables
     */
    public int getMaxNewVars() {
        return maxNewVars;
    }

    /**
     * Returns the user types minimal frequency value
     * 
     * @return minimal frequency value
     */
    public int getMinFreq() {
        return minFreq;
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
