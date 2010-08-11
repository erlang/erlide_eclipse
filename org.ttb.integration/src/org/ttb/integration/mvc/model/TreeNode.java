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

    @Override
    public ITreeNode getParent() {
        return parent;
    }

    @Override
    public void setParent(ITreeNode parent) {
        this.parent = parent;
    }

    @Override
    public String getLabel() {
        return label;
    }

    @Override
    public boolean hasChildren() {
        return (children != null && children.size() > 0);
    }

    @Override
    public List<ITreeNode> getChildren() {
        return children;
    }

    @Override
    public void addChildren(ITreeNode... childrenList) {
        for (ITreeNode child : childrenList) {
            children.add(child);
        }
    }

    @Override
    public void removeChild(ITreeNode child) {
        children.remove(child);
    }

    @Override
    public void setLabel(String label) {
        this.label = label;
    }

    @Override
    public void setImage(Image image) {
        this.image = image;
    }

    @Override
    public Image getImage() {
        return image;
    }
}
