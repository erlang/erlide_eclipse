package org.erlide.eunit.ui.launch.helpers;



import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

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
