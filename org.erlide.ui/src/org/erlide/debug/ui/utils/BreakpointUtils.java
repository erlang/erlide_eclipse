package org.erlide.debug.ui.utils;

import org.eclipse.debug.core.model.ILineBreakpoint;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.util.ErlLogger;

public class BreakpointUtils {

    public static IErlElement getElement(final ILineBreakpoint breakpoint) {
        final IErlElementLocator model = ErlangEngine.getInstance().getModel();
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
