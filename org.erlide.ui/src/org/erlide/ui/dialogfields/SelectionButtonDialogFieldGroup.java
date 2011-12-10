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

import org.eclipse.core.runtime.Assert;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;

/**
 * Dialog field describing a group with buttons (Checkboxes, radio buttons..)
 */
public class SelectionButtonDialogFieldGroup extends DialogField {

    private Composite fButtonComposite;

    private Button[] fButtons;

    private final String[] fButtonNames;

    private final boolean[] fButtonsSelected;

    private final boolean[] fButtonsEnabled;

    private final int fGroupBorderStyle;

    private final int fGroupNumberOfColumns;

    private final int fButtonsStyle;

    /**
     * Creates a group without border.
     */
    public SelectionButtonDialogFieldGroup(final int buttonsStyle,
            final String[] buttonNames, final int nColumns) {
        this(buttonsStyle, buttonNames, nColumns, SWT.NONE);
    }

    /**
     * Creates a group with border (label in border). Accepted button styles
     * are: SWT.RADIO, SWT.CHECK, SWT.TOGGLE For border styles see
     * <code>Group</code>
     */
    public SelectionButtonDialogFieldGroup(final int buttonsStyle,
            final String[] buttonNames, final int nColumns,
            final int borderStyle) {
        super();

        Assert.isTrue(buttonsStyle == SWT.RADIO || buttonsStyle == SWT.CHECK
                || buttonsStyle == SWT.TOGGLE);
        fButtonNames = buttonNames;

        final int nButtons = buttonNames.length;
        fButtonsSelected = new boolean[nButtons];
        fButtonsEnabled = new boolean[nButtons];
        for (int i = 0; i < nButtons; i++) {
            fButtonsSelected[i] = false;
            fButtonsEnabled[i] = true;
        }
        if (buttonsStyle == SWT.RADIO) {
            fButtonsSelected[0] = true;
        }

        fGroupBorderStyle = borderStyle;
        fGroupNumberOfColumns = nColumns <= 0 ? nButtons : nColumns;

        fButtonsStyle = buttonsStyle;

    }

    // ------- layout helpers

    /*
     * @see DialogField#doFillIntoGrid
     */
    @Override
    public Control[] doFillIntoGrid(final Composite parent, final int nColumns) {
        assertEnoughColumns(nColumns);

        if (fGroupBorderStyle == SWT.NONE) {
            final Label label = getLabelControl(parent);
            label.setLayoutData(gridDataForLabel(1));

            final Composite buttonsgroup = getSelectionButtonsGroup(parent);
            final GridData gd = new GridData();
            gd.horizontalSpan = nColumns - 1;
            buttonsgroup.setLayoutData(gd);

            return new Control[] { label, buttonsgroup };
        }
        final Composite buttonsgroup = getSelectionButtonsGroup(parent);
        final GridData gd = new GridData();
        gd.horizontalSpan = nColumns;
        buttonsgroup.setLayoutData(gd);

        return new Control[] { buttonsgroup };
    }

    /*
     * @see DialogField#doFillIntoGrid
     */
    @Override
    public int getNumberOfControls() {
        return fGroupBorderStyle == SWT.NONE ? 2 : 1;
    }

    // ------- ui creation

    private Button createSelectionButton(final int index,
            final Composite group, final SelectionListener listener) {
        final Button button = new Button(group, fButtonsStyle | SWT.LEFT);
        button.setFont(group.getFont());
        button.setText(fButtonNames[index]);
        button.setEnabled(isEnabled() && fButtonsEnabled[index]);
        button.setSelection(fButtonsSelected[index]);
        button.addSelectionListener(listener);
        button.setLayoutData(new GridData());
        return button;
    }

    /**
     * Returns the group widget. When called the first time, the widget will be
     * created.
     * 
     * @param parent
     *            The parent composite when called the first time, or
     *            <code>null</code> after.
     */
    public Composite getSelectionButtonsGroup(final Composite parent) {
        if (fButtonComposite == null) {
            assertCompositeNotNull(parent);

            final GridLayout layout = new GridLayout();
            layout.makeColumnsEqualWidth = true;
            layout.numColumns = fGroupNumberOfColumns;

            if (fGroupBorderStyle != SWT.NONE) {
                final Group group = new Group(parent, fGroupBorderStyle);
                if (fLabelText != null && fLabelText.length() > 0) {
                    group.setText(fLabelText);
                }
                fButtonComposite = group;
            } else {
                fButtonComposite = new Composite(parent, SWT.NULL);
                layout.marginHeight = 0;
                layout.marginWidth = 0;
            }

            fButtonComposite.setLayout(layout);

            final SelectionListener listener = new SelectionListener() {

                @Override
                public void widgetDefaultSelected(final SelectionEvent e) {
                    doWidgetSelected(e);
                }

                @Override
                public void widgetSelected(final SelectionEvent e) {
                    doWidgetSelected(e);
                }
            };
            final int nButtons = fButtonNames.length;
            fButtons = new Button[nButtons];
            for (int i = 0; i < nButtons; i++) {
                fButtons[i] = createSelectionButton(i, fButtonComposite,
                        listener);
            }
            final int nRows = nButtons / fGroupNumberOfColumns;
            final int nFillElements = nRows * fGroupNumberOfColumns - nButtons;
            for (int i = 0; i < nFillElements; i++) {
                createEmptySpace(fButtonComposite);
            }
        }
        return fButtonComposite;
    }

    /**
     * Returns a button from the group or <code>null</code> if not yet created.
     */
    public Button getSelectionButton(final int index) {
        if (index >= 0 && index < fButtons.length) {
            return fButtons[index];
        }
        return null;
    }

    protected void doWidgetSelected(final SelectionEvent e) {
        final Button button = (Button) e.widget;
        for (int i = 0; i < fButtons.length; i++) {
            if (fButtons[i] == button) {
                fButtonsSelected[i] = button.getSelection();
                dialogFieldChanged();
                return;
            }
        }
    }

    // ------ model access

    /**
     * Returns the selection state of a button contained in the group.
     * 
     * @param index
     *            The index of the button
     */
    public boolean isSelected(final int index) {
        if (index >= 0 && index < fButtonsSelected.length) {
            return fButtonsSelected[index];
        }
        return false;
    }

    /**
     * Sets the selection state of a button contained in the group.
     */
    public void setSelection(final int index, final boolean selected) {
        if (index >= 0 && index < fButtonsSelected.length) {
            if (fButtonsSelected[index] != selected) {
                fButtonsSelected[index] = selected;
                if (fButtons != null) {
                    final Button button = fButtons[index];
                    if (isOkToUse(button)) {
                        button.setSelection(selected);
                    }
                }
            }
        }
    }

    // ------ enable / disable management

    @Override
    protected void updateEnableState() {
        super.updateEnableState();
        if (fButtons != null) {
            final boolean enabled = isEnabled();
            for (int i = 0; i < fButtons.length; i++) {
                final Button button = fButtons[i];
                if (isOkToUse(button)) {
                    button.setEnabled(enabled && fButtonsEnabled[i]);
                }
            }
        }
    }

    /**
     * Sets the enable state of a button contained in the group.
     */
    public void enableSelectionButton(final int index, final boolean enable) {
        if (index >= 0 && index < fButtonsEnabled.length) {
            fButtonsEnabled[index] = enable;
            if (fButtons != null) {
                final Button button = fButtons[index];
                if (isOkToUse(button)) {
                    button.setEnabled(isEnabled() && enable);
                }
            }
        }
    }

    @Override
    public void refresh() {
        super.refresh();
        for (int i = 0; i < fButtons.length; i++) {
            final Button button = fButtons[i];
            if (isOkToUse(button)) {
                button.setSelection(fButtonsSelected[i]);
            }
        }
    }

}
