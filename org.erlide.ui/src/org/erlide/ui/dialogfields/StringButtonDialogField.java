/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.dialogfields;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * Dialog field containing a label, text control and a button control.
 */
public class StringButtonDialogField extends StringDialogField {

    private Button fBrowseButton;

    private String fBrowseButtonLabel;

    private final IStringButtonAdapter fStringButtonAdapter;

    private boolean fButtonEnabled;

    public StringButtonDialogField(final IStringButtonAdapter adapter) {
        super();
        fStringButtonAdapter = adapter;
        fBrowseButtonLabel = "!Browse...!"; //$NON-NLS-1$
        fButtonEnabled = true;
    }

    /**
     * Sets the label of the button.
     */
    public void setButtonLabel(final String label) {
        fBrowseButtonLabel = label;
    }

    // ------ adapter communication

    /**
     * Programmatical pressing of the button
     */
    public void changeControlPressed() {
        fStringButtonAdapter.changeControlPressed(this);
    }

    // ------- layout helpers

    /*
     * @see DialogField#doFillIntoGrid
     */
    @Override
    public Control[] doFillIntoGrid(final Composite parent, final int nColumns) {
        assertEnoughColumns(nColumns);

        final Label label = getLabelControl(parent);
        label.setLayoutData(gridDataForLabel(1));
        final Text text = getTextControl(parent);
        text.setLayoutData(gridDataForText(nColumns - 2));
        final Button button = getChangeControl(parent);
        button.setLayoutData(gridDataForButton(button, 1));

        return new Control[] { label, text, button };
    }

    /*
     * @see DialogField#getNumberOfControls
     */
    @Override
    public int getNumberOfControls() {
        return 3;
    }

    protected static GridData gridDataForButton(final Button button,
            final int span) {
        final GridData gd = new GridData();
        gd.horizontalAlignment = GridData.FILL;
        gd.grabExcessHorizontalSpace = false;
        gd.horizontalSpan = span;
        // gd.widthHint = SWTUtil.getButtonWidthHint(button);
        return gd;
    }

    // ------- ui creation

    /**
     * Creates or returns the created buttom widget.
     * 
     * @param parent
     *            The parent composite or <code>null</code> if the widget has
     *            already been created.
     */
    public Button getChangeControl(final Composite parent) {
        if (fBrowseButton == null) {
            assertCompositeNotNull(parent);

            fBrowseButton = new Button(parent, SWT.PUSH);
            fBrowseButton.setFont(parent.getFont());
            fBrowseButton.setText(fBrowseButtonLabel);
            fBrowseButton.setEnabled(isEnabled() && fButtonEnabled);
            fBrowseButton.addSelectionListener(new SelectionListener() {

                @Override
                public void widgetDefaultSelected(final SelectionEvent e) {
                    changeControlPressed();
                }

                @Override
                public void widgetSelected(final SelectionEvent e) {
                    changeControlPressed();
                }
            });

        }
        return fBrowseButton;
    }

    // ------ enable / disable management

    /**
     * Sets the enable state of the button.
     */
    public void enableButton(final boolean enable) {
        if (isOkToUse(fBrowseButton)) {
            fBrowseButton.setEnabled(isEnabled() && enable);
        }
        fButtonEnabled = enable;
    }

    /*
     * @see DialogField#updateEnableState
     */
    @Override
    protected void updateEnableState() {
        super.updateEnableState();
        if (isOkToUse(fBrowseButton)) {
            fBrowseButton.setEnabled(isEnabled() && fButtonEnabled);
        }
    }
}
