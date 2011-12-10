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
public class TreeContentProvider implements
/* ILazyTreeContentProvider */ITreeContentProvider {

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
    public TreeContentProvider(final TreeViewer treeViewer,
            final boolean hasChildrenValue) {
        this.hasChildrenValue = hasChildrenValue;
    }

    @Override
    public void dispose() {
    }

    @Override
    @SuppressWarnings("unchecked")
    public void inputChanged(final Viewer viewer, final Object oldInput,
            final Object newInput) {
        list = (Collection<ITreeNode>) newInput;
    }

    @Override
    public Object[] getChildren(final Object element) {
        return ((ITreeNode) element).getChildren().toArray();
    }

    @Override
    public Object[] getElements(final Object element) {
        return list.toArray();
    }

    @Override
    public Object getParent(final Object element) {
        return ((ITreeNode) element).getParent();
    }

    @Override
    public boolean hasChildren(final Object element) {
        // return ((ITreeNode) element).hasChildren();
        // for optimization purposes always return true
        return hasChildrenValue;
    }
}
