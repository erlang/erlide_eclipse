package org.erlide.tracing.core.mvc.model.treenodes;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.Image;

/**
 * Tree node displayed in treeviewer.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TreeNode implements ITreeNode {

    private ITreeNode parent;
    private String label;
    private final List<ITreeNode> children = new ArrayList<ITreeNode>();
    private Image image;

    public TreeNode() {
        this(null, null);
    }

    public TreeNode(final String label) {
        this(label, null);
    }

    public TreeNode(final String label, final Image image) {
        this.label = label;
        this.image = image;
    }

    public ITreeNode getParent() {
        return parent;
    }

    public void setParent(final ITreeNode parent) {
        this.parent = parent;
    }

    public String getLabel() {
        return label;
    }

    public boolean hasChildren() {
        return children.size() > 0;
    }

    public List<ITreeNode> getChildren() {
        return children;
    }

    public void addChildren(final ITreeNode... childrenList) {
        for (final ITreeNode child : childrenList) {
            children.add(child);
        }
    }

    public void removeChild(final ITreeNode child) {
        children.remove(child);
    }

    public void setLabel(final String label) {
        this.label = label;
    }

    public void setImage(final Image image) {
        this.image = image;
    }

    public Image getImage() {
        return image;
    }
}
