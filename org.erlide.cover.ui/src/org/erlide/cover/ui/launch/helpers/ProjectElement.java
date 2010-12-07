package org.erlide.cover.ui.launch.helpers;

import org.eclipse.swt.graphics.Image;

public class ProjectElement {

    private Image image;
    private String label;
    
    public ProjectElement(String label, Image image) {
        this.image = image;
        this.label = label;
    }
    
    public Image getImage() {
        return image;
    }
    
    public String getLabel() {
        return label;
    }
    
    public String toString() {
        return label;
    }
    
}
