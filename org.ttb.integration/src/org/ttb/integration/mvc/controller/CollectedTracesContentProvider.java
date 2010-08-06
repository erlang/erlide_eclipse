package org.ttb.integration.mvc.controller;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.ttb.integration.mvc.model.CollectedDataList;
import org.ttb.integration.mvc.model.ITreeNode;

/**
 * Content provider for tree view of collected traces.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class CollectedTracesContentProvider implements ITreeContentProvider {

    @Override
    public void dispose() {
    }

    @Override
    public void inputChanged(Viewer arg0, Object arg1, Object arg2) {
    }

    @Override
    public Object[] getChildren(Object element) {
        return ((ITreeNode) element).getChildren().toArray();
    }

    @Override
    public Object[] getElements(Object element) {
        return ((CollectedDataList) element).getData().toArray();
    }

    @Override
    public Object getParent(Object element) {
        return ((ITreeNode) element).getParent();
    }

    @Override
    public boolean hasChildren(Object element) {
        return ((ITreeNode) element).hasChildren();
    }
}
