package org.erlide.tracing.core.mvc.view;

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

    @Override
    public Object[] getElements(final Object inputElement) {
        return backend.getTracePatternsArray();
    }

    @Override
    public void dispose() {
    }

    @Override
    public void inputChanged(final Viewer viewer, final Object oldInput,
            final Object newInput) {
        backend = (TraceBackend) newInput;
    }
}
