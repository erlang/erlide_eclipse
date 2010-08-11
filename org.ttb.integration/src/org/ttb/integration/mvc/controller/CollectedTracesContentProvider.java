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

    public void dispose() {
    }

    public void inputChanged(Viewer arg0, Object arg1, Object arg2) {
    }

    public Object[] getChildren(Object element) {
        return ((ITreeNode) element).getChildren().toArray();
    }

    public Object[] getElements(Object element) {
        return ((CollectedDataList) element).getData().toArray();
    }

    public Object getParent(Object element) {
        return ((ITreeNode) element).getParent();
    }

    public boolean hasChildren(Object element) {
        return ((ITreeNode) element).hasChildren();
    }
}
