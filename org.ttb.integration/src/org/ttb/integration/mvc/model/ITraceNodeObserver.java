package org.ttb.integration.mvc.model;

import org.ttb.integration.TracingStatus;

/**
 * Interface which should be implemented by every class which wants to be
 * notified about tracing or loading.
 * 
 * @author Piotr Dorobisz
 * 
 */
public interface ITraceNodeObserver {

    /**
     * This method is invoked after starting tracing.
     */
    public void startTracing();

    /**
     * This method is invoked after finishing loading file.
     * 
     * @param status
     *            status
     */
    public void finishLoadingFile(TracingStatus status);

    /**
     * This method is invoked after finishing loading traces from file.
     * 
     * @param status
     *            status
     */
    public void finishLoadingTraces(TracingStatus status);

    /**
     * This method is invoked after removing all items from all lists that
     * contain information about loaded traces.
     */
    public void clearTraceLists();
}
