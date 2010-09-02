package org.ttb.integration.ui.dialogs;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.swt.widgets.Shell;
import org.erlide.jinterface.util.ErlLogger;

/**
 * Busy dialog that can be shown while performing time consuming action. Unlike
 * {@link org.eclipse.jface.dialogs.ProgressMonitorDialog} it does not require
 * specifying task to display its progress. To show busy dialog call
 * {@link #start()} and to hide it call {@link #finish()}.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class BusyDialog extends ProgressMonitorDialog {

    private boolean done = false;
    private final Task task;

    /**
     * Creates busy dialog.
     * 
     * @param parent
     *            parent
     * @param label
     *            label
     */
    public BusyDialog(Shell parent, String label) {
        super(parent);
        task = new Task(label);
    }

    /**
     * Shows busy dialog.
     */
    public void start() {
        try {
            done = false;
            run(true, true, task);
        } catch (Exception e) {
            ErlLogger.error(e);
        }
    }

    /**
     * Hides busy dialog.
     */
    public void finish() {
        done = true;
        synchronized (task) {
            task.notify();
        }
    }

    private class Task implements IRunnableWithProgress {

        private final String label;

        public Task(String label) {
            this.label = label;
        }

        public void run(IProgressMonitor monitor) throws InvocationTargetException, InterruptedException {
            monitor.beginTask(label, 0);
            synchronized (this) {
                while (!done) {
                    wait();
                }
            }
            monitor.done();
        }
    }
}
