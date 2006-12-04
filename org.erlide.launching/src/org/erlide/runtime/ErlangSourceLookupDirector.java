package org.erlide.runtime;

import org.eclipse.debug.core.model.IPersistableSourceLocator;
import org.eclipse.debug.core.sourcelookup.AbstractSourceLookupDirector;
import org.eclipse.debug.core.sourcelookup.ISourceLookupParticipant;

public class ErlangSourceLookupDirector extends AbstractSourceLookupDirector
		implements IPersistableSourceLocator {

	public void initializeParticipants() {
		addParticipants(new ISourceLookupParticipant[] { new ErlangSourceLookupParticipant() });
	}

}
