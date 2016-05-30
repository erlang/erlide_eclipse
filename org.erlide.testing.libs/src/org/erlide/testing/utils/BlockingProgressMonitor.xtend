package org.erlide.testing.utils

import java.util.concurrent.CountDownLatch

import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.NullProgressMonitor

/**
 * A progress monitor that can be used to block until the associated operation is complete.
 */
class BlockingProgressMonitor extends NullProgressMonitor {
    val latch = new CountDownLatch(1)

    /**
     * Blocks until done, or the monitor is cancelled.
     */
    def void waitUntilDone() {
        latch.await()
        if (isCanceled())
            throw new BlockingProgressMonitorCancellationException
    }

    override void done() {
        latch.countDown()
    }

    override void setCanceled(boolean cancelled) {
        super.setCanceled(cancelled)
        if (cancelled)
            latch.countDown()
    }

    /**
     * Executes the given operation with a [[IProgressMonitor]] and waits until it is done or cancelled.
     */
    def <T> waitUntilDone((IProgressMonitor)=>T op) {
        val monitor = new BlockingProgressMonitor
        val res = op.apply(monitor)
        monitor.waitUntilDone()
        res
    }
}

class BlockingProgressMonitorCancellationException extends RuntimeException {
    new() {
        super("Operation cancelled")
    }
}
