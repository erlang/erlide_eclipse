package org.ttb.integration.mvc.model;

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

    public TreeNode(String label) {
        this(label, null);
    }

    public TreeNode(String label, Image image) {
        this.label = label;
        this.image = image;
    }

    public ITreeNode getParent() {
        return parent;
    }

    public void setParent(ITreeNode parent) {
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

    public void addChildren(ITreeNode... childrenList) {
        for (ITreeNode child : childrenList) {
            children.add(child);
        }
    }

    public void removeChild(ITreeNode child) {
        children.remove(child);
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public void setImage(Image image) {
        this.image = image;
    }

    public Image getImage() {
        return image;
    }
}
