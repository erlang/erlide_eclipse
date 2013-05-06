package org.erlide.ui;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.ui.progress.UIJob;
import org.erlide.ui.util.PopupDialog;
import org.erlide.util.MessageReporter;

public class UIMessageReporter extends MessageReporter {

    public UIMessageReporter() {
    }

    @Override
    public void displayMessage(final MessageType type, final String message,
            final ReporterPosition style) {
        new UIJob("erlide message") {
            @Override
            public IStatus runInUIThread(final IProgressMonitor monitor) {
                int icon = SWT.NONE;
                if (type == MessageType.ERROR) {
                    icon = SWT.ICON_ERROR;
                } else if (type == MessageType.WARNING) {
                    icon = SWT.ICON_WARNING;
                } else {
                    icon = SWT.ICON_INFORMATION;
                }
                switch (style) {
                case CENTER:
                    PopupDialog.showDialog("erlide " + type, message, icon);
                    break;
                case CORNER:
                    PopupDialog.showBalloon("erlide " + type, message, icon);
                    break;
                default:
                    break;
                }
                return Status.OK_STATUS;
            }
        }.schedule();
    }
}
