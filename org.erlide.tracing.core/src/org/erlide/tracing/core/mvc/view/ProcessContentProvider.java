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

    @Override
    public void dispose() {
    }

    @Override
    public void inputChanged(final Viewer viewer, final Object oldInput,
            final Object newInput) {
    }

    @Override
    public Object[] getElements(final Object inputElement) {
        return (Object[]) inputElement;
    }
}
