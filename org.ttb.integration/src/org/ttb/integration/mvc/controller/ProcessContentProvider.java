package org.ttb.integration.mvc.controller;

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
    public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
    }

    @Override
    public Object[] getElements(Object inputElement) {
        return (Object[]) inputElement;
    }
}
