package org.erlide.ui.util;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PartInitException;

/**
 * Utility class to help with dialogs.
 * <p>
 * Note that a copy of this class exists in the org.eclipse.ui.internal package.
 * </p>
 */
public final class DialogUtil {

    /**
     * Prevent instantiation.
     */
    private DialogUtil() {
    }

    /**
     * Open an error style dialog for PartInitException by including any extra
     * information from the nested CoreException if present.
     */
    public static void openError(final Shell parent, final String title,
            final String message, final PartInitException exception) {
        // Check for a nested CoreException
        CoreException nestedException = null;
        final IStatus status = exception.getStatus();
        if (status != null && status.getException() instanceof CoreException) {
            nestedException = (CoreException) status.getException();
        }

        if (nestedException != null) {
            // Open an error dialog and include the extra
            // status information from the nested CoreException
            ErrorDialog.openError(parent, title, message,
                    nestedException.getStatus());
        } else {
            // Open a regular error dialog since there is no
            // extra information to display
            MessageDialog.openError(parent, title, message);
        }
    }

    /**
     * Removes the '&' accelerator indicator from a label, if any. Also removes
     * the () accelerators which are used in Asian languages.
     */
    public static String removeAccel(String label) {

        final int startBracket = label.indexOf("(&"); //$NON-NLS-1$
        // Non latin accelerator?
        if (startBracket >= 0) {
            final int endBracket = label.indexOf(')');

            // If there is more than one character it is not an accelerator
            if (endBracket - startBracket == 3) {
                return label.substring(0, startBracket)
                        + label.substring(endBracket + 1);
            }
        }

        final int i = label.indexOf('&');
        if (i >= 0) {
            label = label.substring(0, i) + label.substring(i + 1);
        }

        return label;
    }

    /**
     * Return the number of rows available in the current display using the
     * current font.
     * 
     * @param parent
     *            The Composite whose Font will be queried.
     * @return int The result of the display size divided by the font size.
     */
    public static int availableRows(final Composite parent) {

        final int fontHeight = parent.getFont().getFontData()[0].getHeight();
        final int displayHeight = parent.getDisplay().getClientArea().height;

        return displayHeight / fontHeight;
    }

    /**
     * Return whether or not the font in the parent is the size of a regular
     * font. Typically used to know if a font is smaller than the High Contrast
     * Font. This method is used to make layout decisions based on screen space.
     * 
     * @param parent
     *            The Composite whose Font will be queried.
     * @return boolean. True if there are more than 50 lines of possible text in
     *         the display.
     */
    public static boolean inRegularFontMode(final Composite parent) {

        return availableRows(parent) > 50;
    }
}
