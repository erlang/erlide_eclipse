package org.ttb.integration.mvc.model;

import java.util.List;

import org.eclipse.swt.graphics.Image;

public interface ITreeNode {

    public boolean hasChildren();

    public List<ITreeNode> getChildren();

    public void addChild(ITreeNode child);

    public ITreeNode getParent();

    public String getLabel();

    public Image getImage();
}
