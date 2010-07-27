package org.ttb.integration.mvc.model;

import java.util.ArrayList;
import java.util.List;

/**
 * List containing all patterns. It is a singleton class. It notifies all
 * listeners after every modification.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TracePatternList {

    private final List<TracePattern> list = new ArrayList<TracePattern>();
    private final List<ITracePatternListObserver> listeners = new ArrayList<ITracePatternListObserver>();;
    private static TracePatternList INSTANCE = new TracePatternList();

    private TracePatternList() {
    }

    public static TracePatternList getInstance() {
        return INSTANCE;
    }

    public void addListener(ITracePatternListObserver listener) {
        listeners.add(listener);
    }

    public void removeListener(ITracePatternListObserver listener) {
        listeners.remove(listener);
    }

    public void addPattern(TracePattern tracePattern) {
        list.add(tracePattern);
        for (ITracePatternListObserver listener : listeners) {
            listener.addPattern(tracePattern);
        }
    }

    public void removePattern(TracePattern tracePattern) {
        list.remove(tracePattern);
        for (ITracePatternListObserver listener : listeners) {
            listener.removePattern(tracePattern);
        }
    }

    public void updatePattern(TracePattern tracePattern) {
        for (ITracePatternListObserver listener : listeners) {
            listener.updatePattern(tracePattern);
        }
    }

    public Object[] toArray() {
        return list.toArray();
    }
}
