package org.erlide.tracing.core.mvc.model.treenodes;

import java.util.ArrayList;
import java.util.Arrays;
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
    private final List<ITreeNode> children = new ArrayList<>();
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

    @Override
    public ITreeNode getParent() {
        return parent;
    }

    @Override
    public void setParent(final ITreeNode parent) {
        this.parent = parent;
    }

    @Override
    public String getLabel() {
        return label;
    }

    @Override
    public boolean hasChildren() {
        return !children.isEmpty();
    }

    @Override
    public List<ITreeNode> getChildren() {
        return children;
    }

    @Override
    public void addChildren(final ITreeNode... childrenList) {
        children.addAll(Arrays.asList(childrenList));
    }

    @Override
    public void removeChild(final ITreeNode child) {
        children.remove(child);
    }

    @Override
    public void setLabel(final String label) {
        this.label = label;
    }

    @Override
    public void setImage(final Image image) {
        this.image = image;
    }

    @Override
    public Image getImage() {
        return image;
    }
}
