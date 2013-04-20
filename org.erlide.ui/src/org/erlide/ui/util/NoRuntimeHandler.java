package org.erlide.ui.util;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.progress.UIJob;
import org.erlide.util.ErlLogger;
import org.erlide.util.MessageReporter;
import org.erlide.util.MessageReporter.ReporterPosition;

public class NoRuntimeHandler implements Runnable {

    private static boolean running = false;
    private static boolean reported = false;

    @Override
    public void run() {
        final UIJob job = new UIJob("erlide set runtime") {
            @Override
            public IStatus runInUIThread(final IProgressMonitor monitor) {
                if (running) {
                    return Status.OK_STATUS;
                }
                running = true;
                if (reported) {
                    MessageReporter.showError(
                            "Erlang support requires an Erlang installation. "
                                    + "Did you configure it?",
                            ReporterPosition.CORNER);
                } else {
                    MessageReporter.showError(
                            "Erlang support requires an Erlang installation. "
                                    + "Please configure it. "
                                    + "Eclipse will restart afterwards.",
                            ReporterPosition.CORNER);
                }
                reported = true;

                final PreferenceDialog pref = PreferencesUtil
                        .createPreferenceDialogOn(PlatformUI.getWorkbench()
                                .getActiveWorkbenchWindow().getShell(),
                                "org.erlide.ui.preferences.runtimes", null,
                                null);
                if (pref != null) {
                    if (pref.open() == Window.OK) {
                        ErlLogger
                                .info("Restarting workbench after initial runtime configuration...");
                        PlatformUI.getWorkbench().restart();
                    }
                }
                running = false;
                return Status.OK_STATUS;
            }
        };
        job.schedule();
    }
}
