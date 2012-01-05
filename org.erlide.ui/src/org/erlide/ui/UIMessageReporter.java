package org.erlide.ui;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.progress.UIJob;
import org.erlide.core.MessageReporter;
import org.erlide.ui.util.PopupDialog;

public class UIMessageReporter extends MessageReporter {

    public UIMessageReporter() {
    }

    @Override
    public void displayMessage(final MessageType type, final String message,
            final ReporterPosition style) {
        new UIJob("crashed backend") {
            @Override
            public IStatus runInUIThread(final IProgressMonitor monitor) {
                switch (style) {
                case MODAL:
                    PopupDialog.showModalDialog("erlide " + type, message);
                    break;
                case CENTER:
                    PopupDialog.showDialog("erlide " + type, message, 3000);
                    break;
                case CORNER:
                    PopupDialog.showBalloon("erlide " + type, message, 3000);
                    break;
                default:
                    break;
                }
                return Status.OK_STATUS;
            }
        }.schedule();
    }
}
