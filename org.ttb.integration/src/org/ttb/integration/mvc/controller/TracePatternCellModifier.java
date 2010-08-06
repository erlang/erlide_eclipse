package org.ttb.integration.mvc.controller;

import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.swt.widgets.TableItem;
import org.ttb.integration.TtbBackend;
import org.ttb.integration.mvc.model.TracePattern;
import org.ttb.integration.mvc.view.TracePatternColumn;

/**
 * Cell modifier for trace patterns table.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TracePatternCellModifier implements ICellModifier {

    @Override
    public boolean canModify(Object element, String property) {
        return true;
    }

    @Override
    public Object getValue(Object element, String property) {
        TracePattern pattern = (TracePattern) element;
        switch (TracePatternColumn.valueOf(property)) {
        case ENABLED:
            return new Boolean(pattern.isEnabled());
        case MODULE_NAME:
            return pattern.getModuleName();
        case FUNCTION_NAME:
            return pattern.getFunctionName();
        case LOCAL:
            return new Boolean(pattern.isLocal());
        default:
            return null;
        }
    }

    @Override
    public void modify(Object element, String property, Object value) {
        TracePattern pattern = (TracePattern) ((TableItem) element).getData();
        switch (TracePatternColumn.valueOf(property)) {
        case ENABLED:
            pattern.setEnabled((Boolean) value);
            break;
        case MODULE_NAME:
            pattern.setModuleName((String) value);
            break;
        case FUNCTION_NAME:
            pattern.setFunctionName((String) value);
            break;
        case LOCAL:
            pattern.setLocal((Boolean) value);
            break;
        default:
        }
        TtbBackend.getInstance().updateTracePattern(pattern);
    }
}
