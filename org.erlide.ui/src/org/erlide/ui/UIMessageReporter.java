package org.erlide.ui;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.progress.UIJob;
import org.eclipse.ui.statushandlers.StatusManager;
import org.erlide.util.MessageReporter;

public class UIMessageReporter extends MessageReporter {

    public UIMessageReporter() {
    }

    @Override
    public void displayMessage(final int severity, final String message,
            final String details) {
        new UIJob("erlide message") {
            @Override
            public IStatus runInUIThread(final IProgressMonitor monitor) {
                final MultiStatus msg = new MultiStatus("org.erlide.ui", 0,
                        message, null);
                if (details != null) {
                    msg.add(new Status(severity, "org.erlide.ui", details));
                }
                StatusManager.getManager().handle(msg, StatusManager.BLOCK);

                return Status.OK_STATUS;
            }
        }.schedule();
    }
}
