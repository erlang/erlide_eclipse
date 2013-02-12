package org.erlide.launch;

import org.eclipse.debug.core.sourcelookup.AbstractSourceLookupDirector;
import org.eclipse.debug.core.sourcelookup.ISourceLookupParticipant;

public class ErlangSourceLookupDirector extends AbstractSourceLookupDirector {

    @Override
    public void initializeParticipants() {
        addParticipants(new ISourceLookupParticipant[] { new ErlangSourceLookupParticipant() });
    }

}
