package org.erlide.cover.ui.launch.helpers;

import org.eclipse.swt.graphics.Image;

/**
 * Represents project to show it in a browser
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class ProjectElement {

    private final Image image;
    private final String label;

    public ProjectElement(final String label, final Image image) {
        this.image = image;
        this.label = label;
    }

    public Image getImage() {
        return image;
    }

    public String getLabel() {
        return label;
    }

    @Override
    public String toString() {
        return label;
    }

}
