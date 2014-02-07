package org.erlide.ui;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.progress.UIJob;
import org.eclipse.ui.statushandlers.StatusManager;
import org.erlide.util.ErlideMessage;

import com.google.common.eventbus.Subscribe;

public class UIMessageReporter {

    @Subscribe
    public void displayMessage(final ErlideMessage emsg) {
        try {
            new UIJob("erlide message") {
                @Override
                public IStatus runInUIThread(final IProgressMonitor monitor) {
                    final MultiStatus msg = new MultiStatus("org.erlide.ui", 0,
                            emsg.getMessage(), null);
                    if (emsg.getDetails() != null) {
                        msg.add(new Status(emsg.getSeverity(), "org.erlide.ui", emsg
                                .getDetails()));
                    }
                    StatusManager.getManager().handle(msg, StatusManager.BLOCK);

                    return Status.OK_STATUS;
                }
            }.schedule();
        } catch (final IllegalStateException e) {
        }
    }
}
