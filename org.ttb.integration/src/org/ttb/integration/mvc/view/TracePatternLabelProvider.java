package org.ttb.integration.mvc.view;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.ttb.integration.Activator;
import org.ttb.integration.Images;
import org.ttb.integration.mvc.model.TracePattern;

/**
 * Provider responsible for displaying trace patterns in table.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TracePatternLabelProvider extends LabelProvider implements ITableLabelProvider {

    @Override
    public Image getColumnImage(Object element, int index) {
        TracePattern pattern = (TracePattern) element;
        if (index == TracePatternColumn.ENABLED.ordinal()) {
            if (pattern.isEnabled())
                return Activator.getDefault().getImageRegistry().get(Images.CHECKED.toString());
            else
                return Activator.getDefault().getImageRegistry().get(Images.UNCHECKED.toString());
        } else if (index == TracePatternColumn.LOCAL.ordinal()) {
            if (pattern.isLocal())
                return Activator.getDefault().getImageRegistry().get(Images.CHECKED.toString());
            else
                return Activator.getDefault().getImageRegistry().get(Images.UNCHECKED.toString());
        } else
            return null;
    }

    @Override
    public String getColumnText(Object element, int index) {
        TracePattern tracePattern = (TracePattern) element;
        switch (TracePatternColumn.getByIndex(index)) {
        case ENABLED:
            break;
        case MODULE_NAME:
            return tracePattern.getModuleName();
        case FUNCTION_NAME:
            return tracePattern.getFunctionName();
        case LOCAL:
            break;
        default:
            break;
        }
        return "";
    }
}
