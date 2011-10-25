package org.erlide.cover.ui.views.helpers;

import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.erlide.cover.views.model.TestTreeObject;

/**
 * Label provider for EUnit test view
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 *
 */
public class TestViewLabelProvider extends LabelProvider {
	
	public Image getImage(Object element) {
		return null;
	}
	
	public String getText(Object element) {
		TestTreeObject node = (TestTreeObject) element;
		return node.getDescription();
	}

}
