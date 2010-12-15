package org.erlide.debug.ui.utils;

import org.eclipse.debug.core.model.ILineBreakpoint;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModel;
import org.erlide.core.erlang.IErlModule;
import org.erlide.jinterface.util.ErlLogger;

public class BreakpointUtils {

    public static IErlElement getElement(final ILineBreakpoint breakpoint) {
        final IErlModel model = ErlangCore.getModel();
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
