package org.erlide.testing.utils;

import java.util.concurrent.CountDownLatch;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;

/**
 * A progress monitor that can be used to block until the associated operation is
 * complete.
 */
@SuppressWarnings("all")
public class BlockingProgressMonitor extends NullProgressMonitor {
    private final CountDownLatch latch = new CountDownLatch(1);

    /**
     * Blocks until done, or the monitor is cancelled.
     */
    public void waitUntilDone() {
        try {
            latch.await();
            final boolean _isCanceled = isCanceled();
            if (_isCanceled) {
                throw new BlockingProgressMonitorCancellationException();
            }
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    @Override
    public void done() {
        latch.countDown();
    }

    @Override
    public void setCanceled(final boolean cancelled) {
        super.setCanceled(cancelled);
        if (cancelled) {
            latch.countDown();
        }
    }

    /**
     * Executes the given operation with a [[IProgressMonitor]] and waits until it is done
     * or cancelled.
     */
    public <T extends Object> T waitUntilDone(
            final Function1<? super IProgressMonitor, ? extends T> op) {
        T _xblockexpression = null;
        {
            final BlockingProgressMonitor monitor = new BlockingProgressMonitor();
            final T res = op.apply(monitor);
            monitor.waitUntilDone();
            _xblockexpression = res;
        }
        return _xblockexpression;
    }
}
