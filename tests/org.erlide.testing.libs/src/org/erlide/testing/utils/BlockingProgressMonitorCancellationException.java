package org.erlide.testing.utils;

@SuppressWarnings("all")
public class BlockingProgressMonitorCancellationException extends RuntimeException {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

    public BlockingProgressMonitorCancellationException() {
        super("Operation cancelled");
    }
}
