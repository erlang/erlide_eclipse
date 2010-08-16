package org.ttb.integration.mvc.model;

/**
 * Interface which should be implemented by every class which wants to be
 * notified whenever pattern list changed or trace data has been received.
 * 
 * @author Piotr Dorobisz
 * 
 */
public interface ITraceNodeObserver {

    public void addPattern(TracePattern tracePattern);

    public void removePattern(TracePattern tracePattern);

    public void updatePattern(TracePattern tracePattern);

    /**
     * This method is invoked after starting tracing.
     */
    public void startTracing();

    /**
     * This method is invoked after stopping tracing.
     */
    public void stopTracing();

    /**
     * This method is invoked after receiving new trace data.
     */
    public void receivedTraceData();
}
