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
package org.erlide.ui.util;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.erlide.ui.internal.ErlideUIPlugin;

/**
 * A utility class to work with IStatus.
 */
public class StatusUtil {

    /**
     * Compares two instances of <code>IStatus</code>. The more severe is
     * returned: An error is more severe than a warning, and a warning is more
     * severe than ok. If the two stati have the same severity, the second is
     * returned.
     */
    public static IStatus getMoreSevere(final IStatus s1, final IStatus s2) {
        if (s1.getSeverity() > s2.getSeverity()) {
            return s1;
        }
        return s2;
    }

    /**
     * Finds the most severe status from a array of stati. An error is more
     * severe than a warning, and a warning is more severe than ok.
     */
    public static IStatus getMostSevere(final IStatus[] status) {
        IStatus max = null;
        for (final IStatus curr : status) {
            if (curr.matches(IStatus.ERROR)) {
                return curr;
            }
            if (max == null || curr.getSeverity() > max.getSeverity()) {
                max = curr;
            }
        }
        return max;
    }

    /**
     * Applies the status to the status line of a dialog page.
     */
    public static void applyToStatusLine(final DialogPage page, final IStatus status) {
        String message = status.getMessage();
        switch (status.getSeverity()) {
        case IStatus.OK:
            page.setMessage(message, IMessageProvider.NONE);
            page.setErrorMessage(null);
            break;
        case IStatus.WARNING:
            page.setMessage(message, IMessageProvider.WARNING);
            page.setErrorMessage(null);
            break;
        case IStatus.INFO:
            page.setMessage(message, IMessageProvider.INFORMATION);
            page.setErrorMessage(null);
            break;
        default:
            if (message.length() == 0) {
                message = null;
            }
            page.setMessage(null);
            page.setErrorMessage(message);
            break;
        }
    }

    public static IStatus newStatus(final int severity, final String message,
            final Throwable exception) {

        String statusMessage = message;
        if (message == null || message.trim().length() == 0) {
            if (exception == null) {
                throw new IllegalArgumentException();
            } else if (exception.getMessage() == null) {
                statusMessage = exception.toString();
            } else {
                statusMessage = exception.getMessage();
            }
        }

        return new Status(severity, ErlideUIPlugin.PLUGIN_ID, severity, statusMessage,
                exception);
    }

}
