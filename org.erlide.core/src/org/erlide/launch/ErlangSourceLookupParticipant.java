package org.erlide.launch;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.sourcelookup.AbstractSourceLookupParticipant;
import org.erlide.jinterface.ErlLogger;
import org.erlide.launch.debug.model.ErlangStackFrame;

public class ErlangSourceLookupParticipant extends
        AbstractSourceLookupParticipant {

    public ErlangSourceLookupParticipant() {
        super();
    }

    @Override
    public String getSourceName(final Object object) throws CoreException {
        if (!(object instanceof ErlangStackFrame)) {
            return null;
        }
        final ErlangStackFrame f = (ErlangStackFrame) object;
        ErlLogger.debug("SOURCE for " + f.getName() + ": " + f.getModule());
        return f.getModule() + ".erl";
    }
}
