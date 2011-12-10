package org.erlide.tracing.core.ui.dialogs;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;

/**
 * Abstract class that represents long-running operation which should be run in
 * separate thread (while executing UI may show a progress indicator).
 * Operations to be executed have to implemented in {@link #doAction()} method
 * of inheriting class. After executing them thread will wait for calling
 * {@link #finish()} method. Purpose of it is to have task which can be started
 * from one place in the code and stopped from other.
 * 
 * @author Piotr Dorobisz
 * 
 */
public abstract class RunnableWithProgress implements IRunnableWithProgress {

    private boolean done;
    private final String label;

    public abstract void doAction();

    /**
     * Creates new task.
     * 
     * @param label
     *            description of task that will be shown in progress dialog
     */
    public RunnableWithProgress(final String label) {
        this.label = label;
    }

    @Override
    public void run(final IProgressMonitor monitor)
            throws InvocationTargetException, InterruptedException {
        synchronized (this) {
            monitor.beginTask(label, 0);
            doAction();
            while (!done) {
                wait();
            }
            monitor.done();
        }
    }

    /**
     * Indicates that all actions are finished and executing thread can
     * terminate.
     */
    public synchronized void finish() {
        done = true;
        notify();
    }
}
