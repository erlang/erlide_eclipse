package org.ttb.integration.mvc.controller;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.ttb.integration.TtbBackend;

/**
 * Content provider for trace patterns table.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TracePatternContentProvider implements IStructuredContentProvider {

    public TracePatternContentProvider() {
    }

    @Override
    public Object[] getElements(Object inputElement) {
        return TtbBackend.getInstance().getTracePatternsArray();
    }

    @Override
    public void dispose() {
        // TODO
    }

    @Override
    public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
    }
}
