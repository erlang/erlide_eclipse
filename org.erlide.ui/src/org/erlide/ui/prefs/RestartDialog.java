package org.erlide.ui.prefs;

/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;

/**
 * A dialog which prompts the user to restart after an update operation and
 * provides Yes, No, Continue buttons.
 */
public class RestartDialog extends MessageDialog {
    private static final int CONTINUE = 2;
    private final static String[] yesNo = new String[] { IDialogConstants.YES_LABEL,
            IDialogConstants.NO_LABEL };

    private int buttonId = 0;

    /**
     * Creates a new dialog
     *
     * @see MessageDialog#MessageDialog(org.eclipse.swt.widgets.Shell,
     *      java.lang.String, org.eclipse.swt.graphics.Image, java.lang.String,
     *      int, java.lang.String[], int)
     */
    public RestartDialog(final Shell parent, final String title, final String message) {
        super(parent, title, null, // accept the default window icon
                message, QUESTION, yesNo, 0); // yes
        // is
        // the
        // default
    }

    /**
     * Convenience method to open the Yes/No/Continue question dialog.
     *
     * @param parent
     *            the parent shell of the dialog, or <code>null</code> if none
     * @param restartIsReallyNeeded
     *            when false, the changes are applied to the current config
     * @return <code>true</code> if the user presses YES <code>false</code>
     *         otherwise
     */
    public static boolean openQuestion(final Shell parent) {
        final String title = "Erlide runtime changed";
        final String message = "You will need to restart for the changes to take effect. Would you like to restart now?";
        final RestartDialog dialog = new RestartDialog(parent, title, message);
        final int button = dialog.open();
        return button == 0; // Yes
    }

    /**
     * When a button is pressed, store the preference.
     *
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void buttonPressed(final int id) {
        if (id == 2) {
            buttonId = CONTINUE;
        }

        super.buttonPressed(id);
    }

    /**
     * Returns the user's selection, <code>null</code> if the user hasn't chosen
     * yet.
     *
     * @return the user's selection or <code>null</code>
     */
    public int getResult() {
        return buttonId;
    }
}
