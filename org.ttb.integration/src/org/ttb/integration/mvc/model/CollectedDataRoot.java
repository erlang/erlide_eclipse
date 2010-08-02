package org.ttb.integration.mvc.model;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.Image;
import org.ttb.integration.Activator;
import org.ttb.integration.Images;

public class CollectedDataRoot implements ITreeNode {

    private final List<ITreeNode> children = new ArrayList<ITreeNode>();
    private final String label;

    public CollectedDataRoot(String label) {
        this.label = label;
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
    public ITreeNode getParent() {
        return null;
    }

    @Override
    public String getLabel() {
        return this.label;
    }

    @Override
    public void addChild(ITreeNode child) {
        children.add(child);
    }

    @Override
    public Image getImage() {
        return Activator.getDefault().getImageRegistry().get(Images.TREE_ROOT.toString());
    }
}
