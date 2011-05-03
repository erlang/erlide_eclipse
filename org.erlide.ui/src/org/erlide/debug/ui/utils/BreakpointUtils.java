package org.erlide.debug.ui.utils;

import org.eclipse.debug.core.model.ILineBreakpoint;
import org.erlide.core.CoreScope;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.api.IErlElement;
import org.erlide.core.model.root.api.IErlModel;
import org.erlide.jinterface.ErlLogger;

public class BreakpointUtils {

    public static IErlElement getElement(final ILineBreakpoint breakpoint) {
        final IErlModel model = CoreScope.getModel();
        final IErlElement element = model.findElement(breakpoint.getMarker()
                .getResource());
        if (element instanceof IErlModule) {
            final IErlModule m = (IErlModule) element;
            try {
                m.open(null);
                final int lineNumber = breakpoint.getLineNumber();
                return m.getElementAtLine(lineNumber - 1);
            } catch (final Exception e) {
                ErlLogger.warn(e);
            }
        }
        return null;
    }

}
