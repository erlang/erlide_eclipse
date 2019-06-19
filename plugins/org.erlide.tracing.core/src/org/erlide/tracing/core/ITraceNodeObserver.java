package org.erlide.tracing.core;

/**
 * Interface which should be implemented by every class which wants to be notified about
 * tracing or loading.
 *
 * @author Piotr Dorobisz
 *
 */
public interface ITraceNodeObserver {

    /**
     * This method is invoked after starting tracing.
     */
    void startTracing();

    /**
     * This method is invoked after finishing loading file.
     *
     * @param status
     *            status
     */
    void finishLoadingFile(TracingStatus status);

    /**
     * This method is invoked after finishing loading traces from file.
     *
     * @param status
     *            status
     */
    void finishLoadingTraces(TracingStatus status);

    /**
     * This method is invoked after removing file containing tracing results from list.
     */
    void removeFile();

    /**
     * This method is invoked when list of trace patterns changes.
     */
    void updateTracePatterns();
}
