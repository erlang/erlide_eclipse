package org.ttb.integration.mvc.view;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.ttb.integration.mvc.model.TracePattern;

/**
 * Provider responsible for displaying trace patterns in table.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TracePatternLabelProvider extends LabelProvider implements ITableLabelProvider {

    private static final String CHECKED_IMAGE = "checked";
    private static final String UNCHECKED_IMAGE = "unchecked";

    private static final ImageRegistry imageRegistry = new ImageRegistry();

    static {
        imageRegistry.put(CHECKED_IMAGE, ImageDescriptor.createFromFile(TracePatternLabelProvider.class, CHECKED_IMAGE + ".gif"));
        imageRegistry.put(UNCHECKED_IMAGE, ImageDescriptor.createFromFile(TracePatternLabelProvider.class, UNCHECKED_IMAGE + ".gif"));
    }

    @Override
    public Image getColumnImage(Object element, int index) {
        TracePattern pattern = (TracePattern) element;
        if (index == Columns.ENABLED.ordinal()) {
            if (pattern.isEnabled())
                return imageRegistry.get(CHECKED_IMAGE);
            else
                return imageRegistry.get(UNCHECKED_IMAGE);
        } else
            return null;
    }

    @Override
    public String getColumnText(Object element, int index) {
        TracePattern tracePattern = (TracePattern) element;
        switch (Columns.getByIndex(index)) {
        case ENABLED:
            break;
        case MODULE_NAME:
            return tracePattern.getModuleName();
        case FUNCTION_NAME:
            return tracePattern.getFunctionName();
        default:
            break;
        }
        return "";
    }
}
