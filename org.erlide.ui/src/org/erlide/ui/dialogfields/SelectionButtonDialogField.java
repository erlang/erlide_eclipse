/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
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
import org.erlide.ui.util.SWTUtil;

/**
 * Dialog Field containing a single button such as a radio or checkbox button.
 */
public class SelectionButtonDialogField extends DialogField {

    private Button fButton;

    private boolean fIsSelected;

    private DialogField[] fAttachedDialogFields;

    private final int fButtonStyle;

    /**
     * Creates a selection button. Allowed button styles: SWT.RADIO, SWT.CHECK,
     * SWT.TOGGLE, SWT.PUSH
     */
    public SelectionButtonDialogField(final int buttonStyle) {
        super();
        fIsSelected = false;
        fAttachedDialogFields = null;
        fButtonStyle = buttonStyle;
    }

    /**
     * Attaches a field to the selection state of the selection button. The
     * attached field will be disabled if the selection button is not selected.
     */
    public void attachDialogField(final DialogField dialogField) {
        attachDialogFields(new DialogField[] { dialogField });
    }

    /**
     * Attaches fields to the selection state of the selection button. The
     * attached fields will be disabled if the selection button is not selected.
     */
    public void attachDialogFields(final DialogField[] dialogFields) {
        fAttachedDialogFields = dialogFields;
        for (final DialogField element : dialogFields) {
            element.setEnabled(fIsSelected);
        }
    }

    /**
     * Returns <code>true</code> is teh gived field is attached to the selection
     * button.
     */
    public boolean isAttached(final DialogField editor) {
        if (fAttachedDialogFields != null) {
            for (final DialogField element : fAttachedDialogFields) {
                if (element == editor) {
                    return true;
                }
            }
        }
        return false;
    }

    // ------- layout helpers

    /*
     * @see DialogField#doFillIntoGrid
     */
    @Override
    public Control[] doFillIntoGrid(final Composite parent, final int nColumns) {
        assertEnoughColumns(nColumns);

        final Button button = getSelectionButton(parent);
        final GridData gd = new GridData();
        gd.horizontalSpan = nColumns;
        gd.horizontalAlignment = GridData.FILL;
        if (fButtonStyle == SWT.PUSH) {
            gd.heightHint = SWTUtil.getButtonHeightHint(button);
            gd.widthHint = SWTUtil.getButtonWidthHint(button);
        }

        button.setLayoutData(gd);

        return new Control[] { button };
    }

    /*
     * @see DialogField#getNumberOfControls
     */
    @Override
    public int getNumberOfControls() {
        return 1;
    }

    // ------- ui creation

    /**
     * Returns the selection button widget. When called the first time, the
     * widget will be created.
     * 
     * @param group
     *            The parent composite when called the first time, or
     *            <code>null</code> after.
     */
    public Button getSelectionButton(final Composite group) {
        if (fButton == null) {
            assertCompositeNotNull(group);

            fButton = new Button(group, fButtonStyle);
            fButton.setFont(group.getFont());
            fButton.setText(fLabelText);
            fButton.setEnabled(isEnabled());
            fButton.setSelection(fIsSelected);
            fButton.addSelectionListener(new SelectionListener() {

                @Override
                public void widgetDefaultSelected(final SelectionEvent e) {
                    doWidgetSelected(e);
                }

                @Override
                public void widgetSelected(final SelectionEvent e) {
                    doWidgetSelected(e);
                }
            });
        }
        return fButton;
    }

    protected void doWidgetSelected(final SelectionEvent e) {
        if (isOkToUse(fButton)) {
            changeValue(fButton.getSelection());
        }
    }

    private void changeValue(final boolean newState) {
        if (fIsSelected != newState) {
            fIsSelected = newState;
            if (fAttachedDialogFields != null) {
                boolean focusSet = false;
                for (final DialogField element : fAttachedDialogFields) {
                    element.setEnabled(fIsSelected);
                    if (fIsSelected && !focusSet) {
                        focusSet = element.setFocus();
                    }
                }
            }
            dialogFieldChanged();
        } else if (fButtonStyle == SWT.PUSH) {
            dialogFieldChanged();
        }
    }

    // ------ model access

    /**
     * Returns the selection state of the button.
     */
    public boolean isSelected() {
        return fIsSelected;
    }

    /**
     * Sets the selection state of the button.
     */
    public void setSelection(final boolean selected) {
        changeValue(selected);
        if (isOkToUse(fButton)) {
            fButton.setSelection(selected);
        }
    }

    // ------ enable / disable management

    /*
     * @see DialogField#updateEnableState
     */
    @Override
    protected void updateEnableState() {
        super.updateEnableState();
        if (isOkToUse(fButton)) {
            fButton.setEnabled(isEnabled());
        }
    }

    @Override
    public void refresh() {
        super.refresh();
        if (isOkToUse(fButton)) {
            fButton.setSelection(fIsSelected);
        }
    }

}
