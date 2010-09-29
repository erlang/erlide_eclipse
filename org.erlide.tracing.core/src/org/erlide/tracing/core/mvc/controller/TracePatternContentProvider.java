package org.erlide.tracing.core.mvc.controller;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.tracing.core.TraceBackend;

/**
 * Content provider for trace patterns table.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TracePatternContentProvider implements IStructuredContentProvider {

    TraceBackend backend;

    public TracePatternContentProvider() {
    }

    public Object[] getElements(Object inputElement) {
        return backend.getTracePatternsArray();
    }

    public void dispose() {
    }

    public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
        backend = (TraceBackend) newInput;
    }
}
