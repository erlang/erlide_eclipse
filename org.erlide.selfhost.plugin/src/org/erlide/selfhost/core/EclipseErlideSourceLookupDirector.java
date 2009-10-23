package org.erlide.selfhost.core;

import org.eclipse.debug.core.sourcelookup.AbstractSourceLookupDirector;
import org.eclipse.debug.core.sourcelookup.ISourceLookupParticipant;
import org.eclipse.jdt.launching.sourcelookup.containers.JavaSourceLookupParticipant;
import org.erlide.runtime.launch.ErlangSourceLookupParticipant;

public class EclipseErlideSourceLookupDirector extends
AbstractSourceLookupDirector {

	public void initializeParticipants() {
		addParticipants(new ISourceLookupParticipant[] {
				new JavaSourceLookupParticipant(),
				new ErlangSourceLookupParticipant() });
	}

}
