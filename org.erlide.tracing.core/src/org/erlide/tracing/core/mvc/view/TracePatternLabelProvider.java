package org.erlide.tracing.core.mvc.view;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.erlide.tracing.core.Activator;
import org.erlide.tracing.core.Images;
import org.erlide.tracing.core.mvc.model.TracePattern;

/**
 * Provider responsible for displaying trace patterns in table.
 * 
 * @author Piotr Dorobisz
 * 
 */
public class TracePatternLabelProvider extends LabelProvider implements
        ITableLabelProvider {

    @Override
    public Image getColumnImage(final Object element, final int index) {
        final TracePattern pattern = (TracePattern) element;
        if (index == TracePatternColumn.ENABLED.ordinal()) {
            if (pattern.isEnabled()) {
                return Activator.getDefault().getImageRegistry()
                        .get(Images.CHECKED.toString());
            } else {
                return Activator.getDefault().getImageRegistry()
                        .get(Images.UNCHECKED.toString());
            }
        } else if (index == TracePatternColumn.LOCAL.ordinal()) {
            if (pattern.isLocal()) {
                return Activator.getDefault().getImageRegistry()
                        .get(Images.CHECKED.toString());
            } else {
                return Activator.getDefault().getImageRegistry()
                        .get(Images.UNCHECKED.toString());
            }
        } else {
            return null;
        }
    }

    @Override
    public String getColumnText(final Object element, final int index) {
        final TracePattern tracePattern = (TracePattern) element;
        switch (TracePatternColumn.getByIndex(index)) {
        case ENABLED:
            break;
        case MODULE_NAME:
            return tracePattern.getModuleName();
        case FUNCTION_NAME:
            return tracePattern.getFunctionName();
        case ARITY:
            if (tracePattern.getArity() < 0) {
                return "";
            } else {
                return String.valueOf(tracePattern.getArity());
            }
        case LOCAL:
            break;
        case MATCH_SPEC:
            return tracePattern.getMatchSpec().getFunctionString();
        default:
            break;
        }
        return "";
    }
}
