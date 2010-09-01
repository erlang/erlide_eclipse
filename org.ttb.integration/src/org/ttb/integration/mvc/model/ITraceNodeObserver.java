package org.ttb.integration.mvc.model;

import org.ttb.integration.TracingStatus;

/**
 * Interface which should be implemented by every class which wants to be
 * notified whenever pattern list changed or trace data has been received.
 * 
 * @author Piotr Dorobisz
 * 
 */
public interface ITraceNodeObserver {

    /**
     * This method is invoked after adding new trace pattern.
     * 
     * @param tracePattern
     *            new pattern
     */
    public void addPattern(TracePattern tracePattern);

    /**
     * This method is invoked after removing trace pattern.
     * 
     * @param tracePattern
     *            removed trace pattern
     */
    public void removePattern(TracePattern tracePattern);

    /**
     * This method is invoked after updating trace pattern definition.
     * 
     * @param tracePattern
     *            updated trace pattern
     */
    public void updatePattern(TracePattern tracePattern);

    /**
     * This method is invoked after starting tracing.
     */
    public void startTracing();

    /**
     * This method is invoked after stopping tracing.
     * 
     * @param status
     *            status
     */
    public void stopTracing(TracingStatus status);

    /**
     * This method is invoked after receiving new trace data.
     */
    public void receivedTraceData();

    /**
     * This method is invoked after starting loading data from disk.
     */
    public void startLoading();

    /**
     * This method is invoked after finishing loading data from disk.
     * 
     * @param status
     *            status
     */
    public void stopLoading(TracingStatus status);

    /**
     * This method is invoked after loading new set of trace patterns.
     */
    public void loadPatterns();
}
