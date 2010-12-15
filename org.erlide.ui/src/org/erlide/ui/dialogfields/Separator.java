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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

/**
 * Dialog field describing a separator.
 */
public class Separator extends DialogField {

    private Label fSeparator;

    private final int fStyle;

    public Separator() {
        this(SWT.NONE);
    }

    /**
     * @param style
     *            of the separator. See <code>Label</code> for possible styles.
     */
    public Separator(final int style) {
        super();
        fStyle = style;
    }

    // ------- layout helpers

    /**
     * Creates the separator and fills it in a MGridLayout.
     * 
     * @param height
     *            The height of the separator
     */
    public Control[] doFillIntoGrid(final Composite parent, final int nColumns,
            final int height) {
        assertEnoughColumns(nColumns);

        final Control separator = getSeparator(parent);
        separator.setLayoutData(gridDataForSeperator(nColumns, height));

        return new Control[] { separator };
    }

    /*
     * @see DialogField#doFillIntoGrid
     */
    @Override
    public Control[] doFillIntoGrid(final Composite parent, final int nColumns) {
        return doFillIntoGrid(parent, nColumns, 4);
    }

    /*
     * @see DialogField#getNumberOfControls
     */
    @Override
    public int getNumberOfControls() {
        return 1;
    }

    protected static GridData gridDataForSeperator(final int span,
            final int height) {
        final GridData gd = new GridData();
        gd.horizontalAlignment = GridData.FILL;
        gd.verticalAlignment = GridData.BEGINNING;
        gd.heightHint = height;
        gd.horizontalSpan = span;
        return gd;
    }

    // ------- ui creation

    /**
     * Creates or returns the created separator.
     * 
     * @param parent
     *            The parent composite or <code>null</code> if the widget has
     *            already been created.
     */
    public Control getSeparator(final Composite parent) {
        if (fSeparator == null) {
            assertCompositeNotNull(parent);
            fSeparator = new Label(parent, fStyle);
        }
        return fSeparator;
    }

}
