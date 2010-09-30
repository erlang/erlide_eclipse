package org.erlide.tracing.core.mvc.view;

import java.util.Collection;

import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.erlide.tracing.core.mvc.model.treenodes.ITreeNode;

/**
 * Content provider for tree.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TreeContentProvider implements /* ILazyTreeContentProvider */ITreeContentProvider {

    private final TreeViewer treeViewer;
    private Collection<ITreeNode> list;
    private final boolean hasChildrenValue;

    /**
     * Creates content provider.
     * 
     * @param treeViewer
     *            tree viewer that will be using this content provider
     * @param hasChildrenValue
     *            value which should be returned when
     *            {@link #hasChildren(Object)} is called
     */
    public TreeContentProvider(TreeViewer treeViewer, boolean hasChildrenValue) {
        this.treeViewer = treeViewer;
        this.hasChildrenValue = hasChildrenValue;
    }

    public void dispose() {
    }

    @SuppressWarnings("unchecked")
    public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
        list = (Collection<ITreeNode>) newInput;
    }

    public Object[] getChildren(Object element) {
        return ((ITreeNode) element).getChildren().toArray();
    }

    public Object[] getElements(Object element) {
        return list.toArray();
    }

    public Object getParent(Object element) {
        return ((ITreeNode) element).getParent();
    }

    public boolean hasChildren(Object element) {
        // return ((ITreeNode) element).hasChildren();
        // for optimization purposes always return true
        return hasChildrenValue;
    }
}
