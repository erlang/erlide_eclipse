package org.ttb.integration.mvc.controller;

import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.swt.widgets.TableItem;
import org.ttb.integration.TraceBackend;
import org.ttb.integration.mvc.model.MatchSpec;
import org.ttb.integration.mvc.model.TracePattern;
import org.ttb.integration.mvc.view.TracePatternColumn;

/**
 * Cell modifier for trace patterns table.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TracePatternCellModifier implements ICellModifier {

    public boolean canModify(Object element, String property) {
        return true;
    }

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
        case ARITY:
            if (pattern.getArity() < 0)
                return "";
            else
                return String.valueOf(pattern.getArity());
        case MATCH_SPEC:
            return pattern.getMatchSpec();
        default:
            return null;
        }
    }

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
        case ARITY:
            if (value == null || "".equals(value)) {
                pattern.setArity(-1);
            } else {
                try {
                    Integer arity = Integer.valueOf((String) value);
                    if (arity >= 0)
                        pattern.setArity(arity.intValue());
                } catch (NumberFormatException e) {
                }
            }
            break;
        case MATCH_SPEC:
            pattern.setMatchSpec((MatchSpec) value);
            break;
        default:
        }
        TraceBackend.getInstance().updateTracePattern(pattern);
    }
}
