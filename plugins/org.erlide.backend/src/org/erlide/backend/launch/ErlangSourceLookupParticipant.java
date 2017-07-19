package org.erlide.backend.launch;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.sourcelookup.AbstractSourceLookupParticipant;
import org.erlide.backend.debug.model.ErlangStackFrame;

public class ErlangSourceLookupParticipant extends AbstractSourceLookupParticipant {

    public ErlangSourceLookupParticipant() {
        super();
    }

    @Override
    public String getSourceName(final Object object) throws CoreException {
        if (!(object instanceof ErlangStackFrame)) {
            return null;
        }
        final ErlangStackFrame f = (ErlangStackFrame) object;
        return f.getModule() + ".erl";
    }
}
