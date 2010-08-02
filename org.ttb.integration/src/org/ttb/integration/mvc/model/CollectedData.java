package org.ttb.integration.mvc.model;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.Image;
import org.ttb.integration.Activator;
import org.ttb.integration.Images;

public class CollectedData implements ITreeNode {

    private ITreeNode parent;
    private final String label;
    private final List<ITreeNode> children = new ArrayList<ITreeNode>();

    public CollectedData(String label) {
        this.label = label;
    }

    @Override
    public ITreeNode getParent() {
        return parent;
    }

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
    public void addChild(ITreeNode child) {
        children.add(child);
    }

    @Override
    public Image getImage() {
        return Activator.getDefault().getImageRegistry().get(Images.TREE_NODE.toString());
    }
}
