package org.erlide.tracing.core.mvc.view;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;

/**
 * Content provider for processes table.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class ProcessContentProvider implements IStructuredContentProvider {

    public void dispose() {
    }

    public void inputChanged(final Viewer viewer, final Object oldInput,
            final Object newInput) {
    }

    public Object[] getElements(final Object inputElement) {
        return (Object[]) inputElement;
    }
}
