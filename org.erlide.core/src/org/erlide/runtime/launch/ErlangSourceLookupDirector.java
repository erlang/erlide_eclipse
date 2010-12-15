package org.erlide.runtime.launch;

import org.eclipse.debug.core.sourcelookup.AbstractSourceLookupDirector;
import org.eclipse.debug.core.sourcelookup.ISourceLookupParticipant;

public class ErlangSourceLookupDirector extends AbstractSourceLookupDirector {

    public void initializeParticipants() {
        addParticipants(new ISourceLookupParticipant[] { new ErlangSourceLookupParticipant() });
    }

}
