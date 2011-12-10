package org.erlide.tracing.core.mvc.view;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.tracing.core.TraceBackend;

/**
 * Content provider for nodes table.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class NodeContentProvider implements IStructuredContentProvider {

    TraceBackend backend;

    public NodeContentProvider() {
    }

    @Override
    public Object[] getElements(final Object inputElement) {
        return backend.getTracedNodesArray();
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
