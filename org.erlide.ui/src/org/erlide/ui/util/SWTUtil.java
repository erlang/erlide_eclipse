/*******************************************************************************
 * Copyright (c) 2000, 2003 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.util;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Caret;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.ScrollBar;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Widget;

/**
 * Utility class to simplify access to some SWT resources.
 */
public final class SWTUtil {

    private SWTUtil() {
    }

    /**
     * Returns the standard display to be used. The method first checks, if the
     * thread calling this method has an associated disaply. If so, this display
     * is returned. Otherwise the method returns the default display.
     */
    public static Display getStandardDisplay() {
        Display display;
        display = Display.getCurrent();
        if (display == null) {
            display = Display.getDefault();
        }
        return display;
    }

    /**
     * Returns the shell for the given widget. If the widget doesn't represent a
     * SWT object that manage a shell, <code>null</code> is returned.
     * 
     * @return the shell for the given widget
     */
    public static Shell getShell(final Widget widget) {
        if (widget instanceof Control) {
            return ((Control) widget).getShell();
        }
        if (widget instanceof Caret) {
            return ((Caret) widget).getParent().getShell();
        }
        if (widget instanceof DragSource) {
            return ((DragSource) widget).getControl().getShell();
        }
        if (widget instanceof DropTarget) {
            return ((DropTarget) widget).getControl().getShell();
        }
        if (widget instanceof Menu) {
            return ((Menu) widget).getParent().getShell();
        }
        if (widget instanceof ScrollBar) {
            return ((ScrollBar) widget).getParent().getShell();
        }

        return null;
    }

    /**
     * Returns a width hint for a button control.
     */
    public static int getButtonWidthHint(final Button button) {
        final PixelConverter converter = new PixelConverter(button);
        final int widthHint = converter
                .convertHorizontalDLUsToPixels(IDialogConstants.BUTTON_WIDTH);
        return Math.max(widthHint,
                button.computeSize(SWT.DEFAULT, SWT.DEFAULT, true).x);
    }

    /**
     * Returns a height hint for a button control.
     */
    public static int getButtonHeightHint(final Button button) {
        return 30;
    }

    /**
     * Sets width and height hint for the button control. <b>Note:</b> This is a
     * NOP if the button's layout data is not an instance of
     * <code>GridData</code>.
     * 
     * @param the
     *            button for which to set the dimension hint
     */
    public static void setButtonDimensionHint(final Button button) {
        Assert.isNotNull(button);
        final Object gd = button.getLayoutData();
        if (gd instanceof GridData) {
            final GridData gridData = (GridData) gd;
            gridData.heightHint = getButtonHeightHint(button);
            gridData.widthHint = getButtonWidthHint(button);
            gridData.horizontalAlignment = GridData.FILL;
        }
    }

    /**
     * Creates and returns a new push button with the given label and/or image.
     * 
     * @param parent
     *            parent control
     * @param label
     *            button label or <code>null</code>
     * @param image
     *            image of <code>null</code>
     * 
     * @return a new push button
     */
    public static Button createPushButton(final Composite parent,
            final String label, final Image image) {
        final Button button = createButton(parent, label, SWT.PUSH);
        if (image != null) {
            button.setImage(image);
        }
        return button;
    }

    /**
     * Creates and returns a new radio button with the given label.
     * 
     * @param parent
     *            parent control
     * @param label
     *            button label or <code>null</code>
     * 
     * @return a new radio button
     */
    public static Button createRadioButton(final Composite parent,
            final String label) {
        final Button button = createButton(parent, label, SWT.RADIO);
        return button;
    }

    /**
     * @param parent
     * @param label
     * @param style
     * @return
     */
    private static Button createButton(final Composite parent,
            final String label, final int style) {
        final Button button = new Button(parent, style);
        button.setFont(parent.getFont());
        if (label != null) {
            button.setText(label);
        }
        final GridData gd = new GridData();
        button.setLayoutData(gd);
        SWTUtil.setButtonDimensionHint(button);
        return button;
    }

    /**
     * @param parent
     * @param label
     * @return
     */
    public static Button createCheckButton(final Composite parent,
            final String label) {
        return createButton(parent, label, SWT.CHECK);
    }

    /**
     * Creates and returns a group with the given characteristics
     * 
     * @param parent
     *            parent control
     * @param title
     *            title for group or <code>null</code>
     * @param numColumns
     *            number of columns in group layout
     * @param fill
     *            {@link GridData} fill style, horizontal, vertical, both or
     *            none
     * 
     * @return the group created
     */
    public static Group createGroup(final Composite parent, final String title,
            final int numColumns, final int fill) {
        final Group group = new Group(parent, SWT.NONE);
        final GridData gd = new GridData(fill);
        group.setLayoutData(gd);
        final GridLayout layout = new GridLayout();
        layout.numColumns = numColumns;
        group.setLayout(layout);
        group.setFont(parent.getFont());
        if (title != null) {
            group.setText(title);
        }
        return group;
    }
}
