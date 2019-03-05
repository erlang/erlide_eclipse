package org.erlide.tracing.core.mvc.model.treenodes;

import java.util.List;

import org.eclipse.swt.graphics.Image;

/**
 * Interface implemented by every element displayed in treeviewer.
 *
 * @author Piotr Dorobisz
 *
 */
public interface ITreeNode {

    boolean hasChildren();

    List<ITreeNode> getChildren();

    void addChildren(ITreeNode... child);

    void removeChild(ITreeNode child);

    ITreeNode getParent();

    void setParent(ITreeNode parent);

    String getLabel();

    void setLabel(String label);

    Image getImage();

    void setImage(Image image);
}
