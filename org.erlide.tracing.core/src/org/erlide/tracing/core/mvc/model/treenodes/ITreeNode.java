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

    public boolean hasChildren();

    public List<ITreeNode> getChildren();

    public void addChildren(ITreeNode... child);

    public void removeChild(ITreeNode child);

    public ITreeNode getParent();

    public void setParent(ITreeNode parent);

    public String getLabel();

    public void setLabel(String label);

    public Image getImage();

    public void setImage(Image image);
}
