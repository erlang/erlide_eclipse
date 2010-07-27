package org.ttb.integration.mvc.controller;

import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.swt.widgets.TableItem;
import org.ttb.integration.mvc.model.TracePattern;
import org.ttb.integration.mvc.model.TracePatternList;
import org.ttb.integration.mvc.view.Columns;

/**
 * Cell modifier for trace patterns table.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class CellModifier implements ICellModifier {

    @Override
    public boolean canModify(Object element, String property) {
        return true;
    }

    @Override
    public Object getValue(Object element, String property) {
        TracePattern pattern = (TracePattern) element;
        switch (Columns.valueOf(property)) {
        case ENABLED:
            return new Boolean(pattern.isEnabled());
        case MODULE_NAME:
            return pattern.getModuleName();
        case FUNCTION_NAME:
            return pattern.getFunctionName();
        default:
            return null;
        }
    }

    @Override
    public void modify(Object element, String property, Object value) {
        TracePattern pattern = (TracePattern) ((TableItem) element).getData();
        switch (Columns.valueOf(property)) {
        case ENABLED:
            pattern.setEnabled((Boolean) value);
            break;
        case MODULE_NAME:
            pattern.setModuleName((String) value);
            break;
        case FUNCTION_NAME:
            pattern.setFunctionName((String) value);
            break;
        default:
        }
        TracePatternList.getInstance().updatePattern(pattern);
    }
}
