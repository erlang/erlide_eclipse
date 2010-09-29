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

    public Object[] getElements(Object inputElement) {
        return backend.getTracedNodesArray();
    }

    public void dispose() {
    }

    public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
        backend = (TraceBackend) newInput;
    }
}
