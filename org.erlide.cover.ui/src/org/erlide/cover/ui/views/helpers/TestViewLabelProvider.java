package org.erlide.cover.ui.views.helpers;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Display;
import org.erlide.cover.views.model.TestTreeObject;

/**
 * Label provider for EUnit test view
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 *
 */
public class TestViewLabelProvider extends LabelProvider {
	
	public Image getImage(Object element) {
		return drawType();
	}
	
	public String getText(Object element) {
		TestTreeObject node = (TestTreeObject) element;
		return node.getDescription();
	}
	
	private Image drawType() {

        final Image img = new Image(Display.getCurrent(), new Rectangle(2, 3,
                20, 20));

        final GC graphic = new GC(img);
        graphic.setForeground(new Color(Display.getCurrent(), 60, 140, 10));
        graphic.setBackground(new Color(Display.getCurrent(), 60, 140, 10));
        graphic.fillOval(3, 3, 14, 14);

        return img;
    }

}
