package org.ttb.integration.mvc.model;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.graphics.Image;

public class CollectedDataRoot implements ITreeNode {

    private final List<ITreeNode> children = new ArrayList<ITreeNode>();
    private final String label;

    // private static final ImageRegistry imageRegistry = new ImageRegistry();
    // private static String IMAGE_NAME = "treeRoot";
    //
    // static {
    // imageRegistry.put(IMAGE_NAME,
    // ImageDescriptor.createFromFile(TracePatternLabelProvider.class,
    // IMAGE_NAME + ".gif"));
    // }

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
        // return imageRegistry.get(IMAGE_NAME);
        return null;
    }
}
