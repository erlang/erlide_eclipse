package org.erlide.tracing.core.mvc.controller;

import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.widgets.TableItem;
import org.erlide.tracing.core.mvc.model.MatchSpec;
import org.erlide.tracing.core.mvc.model.TracePattern;
import org.erlide.tracing.core.mvc.view.TracePatternColumn;

/**
 * Cell modifier for trace patterns table.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TracePatternCellModifier implements ICellModifier {

    private final TableViewer tableViewer;

    /**
     * Creates cell modifier for trace patterns table.
     * 
     * @param tableViewer
     *            table viewer with which this cell modifier is used
     */
    public TracePatternCellModifier(final TableViewer tableViewer) {
        this.tableViewer = tableViewer;
    }

    @Override
    public boolean canModify(final Object element, final String property) {
        return true;
    }

    @Override
    public Object getValue(final Object element, final String property) {
        final TracePattern pattern = (TracePattern) element;
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
            if (pattern.getArity() < 0) {
                return "";
            } else {
                return String.valueOf(pattern.getArity());
            }
        case MATCH_SPEC:
            return pattern.getMatchSpec();
        default:
            return null;
        }
    }

    @Override
    public void modify(final Object element, final String property,
            final Object value) {
        final TracePattern pattern = (TracePattern) ((TableItem) element)
                .getData();
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
                    final Integer arity = Integer.valueOf((String) value);
                    if (arity >= 0) {
                        pattern.setArity(arity.intValue());
                    }
                } catch (final NumberFormatException e) {
                }
            }
            break;
        case MATCH_SPEC:
            pattern.setMatchSpec((MatchSpec) value);
            break;
        default:
        }
        tableViewer.refresh();
    }
}
