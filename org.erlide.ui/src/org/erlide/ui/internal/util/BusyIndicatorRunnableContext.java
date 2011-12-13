/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.internal.util;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.jface.operation.IRunnableContext;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.operation.ModalContext;
import org.eclipse.swt.custom.BusyIndicator;

/**
 * A runnable context that shows the busy cursor instead of a progress monitor.
 * Note, that the UI thread is blocked even if the runnable is executed in a
 * separate thread by passing <code>fork= true</code> to the context's run
 * method. Furthermore this context doesn't provide any UI to cancel the
 * operation.
 */
public class BusyIndicatorRunnableContext implements IRunnableContext {

    private static class BusyRunnable implements Runnable {

        private static class ThreadContext extends Thread {

            IRunnableWithProgress fRunnable;

            Throwable fThrowable;

            public ThreadContext(final IRunnableWithProgress runnable) {
                this(runnable, "BusyCursorRunnableContext-Thread"); //$NON-NLS-1$
            }

            protected ThreadContext(final IRunnableWithProgress runnable,
                    final String name) {
                super(name);
                fRunnable = runnable;
            }

            @Override
            public void run() {
                try {
                    fRunnable.run(new NullProgressMonitor());
                } catch (final InvocationTargetException e) {
                    fThrowable = e;
                } catch (final InterruptedException e) {
                    fThrowable = e;
                } catch (final ThreadDeath e) {
                    fThrowable = e;
                    throw e;
                } catch (final RuntimeException e) {
                    fThrowable = e;
                } catch (final Error e) {
                    fThrowable = e;
                }
            }

            void sync() {
                try {
                    join();
                } catch (final InterruptedException e) {
                    // ok to ignore exception
                }
            }
        }

        public Throwable fThrowable;

        private final boolean fFork;

        private final IRunnableWithProgress fRunnable;

        public BusyRunnable(final boolean fork,
                final IRunnableWithProgress runnable) {
            fFork = fork;
            fRunnable = runnable;
        }

        @Override
        public void run() {
            try {
                internalRun(fFork, fRunnable);
            } catch (final InvocationTargetException e) {
                fThrowable = e;
            } catch (final InterruptedException e) {
                fThrowable = e;
            }
        }

        private void internalRun(boolean fork,
                final IRunnableWithProgress runnable)
                throws InvocationTargetException, InterruptedException {
            final Thread thread = Thread.currentThread();
            // Do not spawn another thread if we are already in a modal context
            // thread or inside a busy context thread.
            if (thread instanceof ThreadContext
                    || ModalContext.isModalContextThread(thread)) {
                fork = false;
            }

            if (fork) {
                final ThreadContext t = new ThreadContext(runnable);
                t.start();
                t.sync();
                // Check if the separate thread was terminated by an exception
                final Throwable throwable = t.fThrowable;
                if (throwable != null) {
                    if (throwable instanceof InvocationTargetException) {
                        throw (InvocationTargetException) throwable;
                    } else if (throwable instanceof InterruptedException) {
                        throw (InterruptedException) throwable;
                    } else if (throwable instanceof OperationCanceledException) {
                        throw new InterruptedException();
                    } else {
                        throw new InvocationTargetException(throwable);
                    }
                }
            } else {
                try {
                    runnable.run(new NullProgressMonitor());
                } catch (final OperationCanceledException e) {
                    throw new InterruptedException();
                }
            }
        }
    }

    @Override
    public void run(final boolean fork, final boolean cancelable,
            final IRunnableWithProgress runnable)
            throws InvocationTargetException, InterruptedException {
        final BusyRunnable busyRunnable = new BusyRunnable(fork, runnable);
        BusyIndicator.showWhile(null, busyRunnable);
        final Throwable throwable = busyRunnable.fThrowable;
        if (throwable instanceof InvocationTargetException) {
            throw (InvocationTargetException) throwable;
        } else if (throwable instanceof InterruptedException) {
            throw (InterruptedException) throwable;
        }
    }
}
