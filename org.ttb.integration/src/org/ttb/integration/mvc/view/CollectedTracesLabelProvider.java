package org.ttb.integration.mvc.view;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.ttb.integration.mvc.model.treenodes.ITreeNode;

/**
 * Label provider for tree view of collected traces.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class CollectedTracesLabelProvider extends LabelProvider {

    @Override
    public Image getImage(Object element) {
        return ((ITreeNode) element).getImage();
    }

    @Override
    public String getText(Object element) {
        return ((ITreeNode) element).getLabel();
    }

    @Override
    public void dispose() {
    }
}
