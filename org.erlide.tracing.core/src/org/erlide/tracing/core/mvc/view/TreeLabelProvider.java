package org.erlide.tracing.core.mvc.view;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.erlide.tracing.core.mvc.model.treenodes.ITreeNode;

/**
 * Label provider for tree.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TreeLabelProvider extends LabelProvider {

    @Override
    public Image getImage(final Object element) {
        return ((ITreeNode) element).getImage();
    }

    @Override
    public String getText(final Object element) {
        return ((ITreeNode) element).getLabel();
    }

    @Override
    public void dispose() {
    }
}
