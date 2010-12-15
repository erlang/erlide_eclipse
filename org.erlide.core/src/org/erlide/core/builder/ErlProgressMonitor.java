/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.builder;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IProgressMonitorWithBlocking;
import org.eclipse.core.runtime.IStatus;

// import com.ericsson.otp.erlang.OtpMbox;

public class ErlProgressMonitor implements IProgressMonitor,
        IProgressMonitorWithBlocking {

    // private OtpMbox mbox;

    /** The wrapped progress monitor. */
    private final IProgressMonitor progressMonitor;

    /**
     * Creates a new wrapper around the given monitor.
     * 
     * @param monitor
     *            the progress monitor to forward to
     */
    protected ErlProgressMonitor(final IProgressMonitor monitor) {
        progressMonitor = monitor;
    }

    /**
     * This implementation of a <code>IProgressMonitor</code> method forwards to
     * the wrapped progress monitor. Clients may override this method to do
     * additional processing.
     * 
     * @see IProgressMonitor#beginTask(String, int)
     */
    public void beginTask(final String name, final int totalWork) {
        progressMonitor.beginTask(name, totalWork);
    }

    /**
     * This implementation of a <code>IProgressMonitorWithBlocking</code> method
     * forwards to the wrapped progress monitor. Clients may override this
     * method to do additional processing.
     * 
     * @see IProgressMonitorWithBlocking#clearBlocked()
     * @since 3.0
     */
    public void clearBlocked() {
        if (progressMonitor instanceof IProgressMonitorWithBlocking) {
            ((IProgressMonitorWithBlocking) progressMonitor).clearBlocked();
        }
    }

    /**
     * This implementation of a <code>IProgressMonitor</code> method forwards to
     * the wrapped progress monitor. Clients may override this method to do
     * additional processing.
     * 
     * @see IProgressMonitor#done()
     */
    public void done() {
        progressMonitor.done();
    }

    /**
     * Returns the wrapped progress monitor.
     * 
     * @return the wrapped progress monitor
     */
    public IProgressMonitor getWrappedProgressMonitor() {
        return progressMonitor;
    }

    /**
     * This implementation of a <code>IProgressMonitor</code> method forwards to
     * the wrapped progress monitor. Clients may override this method to do
     * additional processing.
     * 
     * @see IProgressMonitor#internalWorked(double)
     */
    public void internalWorked(final double work) {
        progressMonitor.internalWorked(work);
    }

    /**
     * This implementation of a <code>IProgressMonitor</code> method forwards to
     * the wrapped progress monitor. Clients may override this method to do
     * additional processing.
     * 
     * @see IProgressMonitor#isCanceled()
     */
    public boolean isCanceled() {
        return progressMonitor.isCanceled();
    }

    /**
     * This implementation of a <code>IProgressMonitorWithBlocking</code> method
     * forwards to the wrapped progress monitor. Clients may override this
     * method to do additional processing.
     * 
     * @see IProgressMonitorWithBlocking#setBlocked(IStatus)
     * @since 3.0
     */
    public void setBlocked(final IStatus reason) {
        if (progressMonitor instanceof IProgressMonitorWithBlocking) {
            ((IProgressMonitorWithBlocking) progressMonitor).setBlocked(reason);
        }
    }

    /**
     * This implementation of a <code>IProgressMonitor</code> method forwards to
     * the wrapped progress monitor. Clients may override this method to do
     * additional processing.
     * 
     * @see IProgressMonitor#setCanceled(boolean)
     */
    public void setCanceled(final boolean b) {
        progressMonitor.setCanceled(b);
    }

    /**
     * This implementation of a <code>IProgressMonitor</code> method forwards to
     * the wrapped progress monitor. Clients may override this method to do
     * additional processing.
     * 
     * @see IProgressMonitor#setTaskName(String)
     */
    public void setTaskName(final String name) {
        progressMonitor.setTaskName(name);
    }

    /**
     * This implementation of a <code>IProgressMonitor</code> method forwards to
     * the wrapped progress monitor. Clients may override this method to do
     * additional processing.
     * 
     * @see IProgressMonitor#subTask(String)
     */
    public void subTask(final String name) {
        progressMonitor.subTask(name);
    }

    /**
     * This implementation of a <code>IProgressMonitor</code> method forwards to
     * the wrapped progress monitor. Clients may override this method to do
     * additional processing.
     * 
     * @see IProgressMonitor#worked(int)
     */
    public void worked(final int work) {
        progressMonitor.worked(work);
    }

}
