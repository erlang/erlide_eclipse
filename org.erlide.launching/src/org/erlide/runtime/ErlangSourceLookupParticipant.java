package org.erlide.runtime;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.sourcelookup.AbstractSourceLookupParticipant;
import org.eclipse.debug.core.sourcelookup.ISourceLookupParticipant;
import org.erlide.runtime.debug.ErlangStackFrame;


public class ErlangSourceLookupParticipant extends AbstractSourceLookupParticipant implements
        ISourceLookupParticipant
{

    public ErlangSourceLookupParticipant()
    {
        super();
        // TODO Auto-generated constructor stub
    }

    public String getSourceName(Object object) throws CoreException
    {
        if (!(object instanceof ErlangStackFrame))
            return null;
        ErlangStackFrame f = (ErlangStackFrame) object;
        return f.getModule();
    }

}
