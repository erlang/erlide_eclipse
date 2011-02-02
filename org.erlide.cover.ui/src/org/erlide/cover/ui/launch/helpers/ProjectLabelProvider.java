package org.erlide.cover.ui.launch.helpers;



import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

/**
 * Label provider for projects
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 *
 */
public class ProjectLabelProvider extends LabelProvider {
    
    public Image getImage(Object element) {
        if(element instanceof ProjectElement)
            return ((ProjectElement)element).getImage();
        return null;
    }
    
    public String getText(Object element) {
        return element.toString();
    }

}
