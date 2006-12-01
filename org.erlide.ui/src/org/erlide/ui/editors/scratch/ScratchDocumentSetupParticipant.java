/**********************************************************************
 Copyright (c) 2000, 2004 IBM Corp. and others.
 All rights reserved. This program and the accompanying materials
 are made available under the terms of the Eclipse Public License v1.0
 which accompanies this distribution, and is available at
 http://www.eclipse.org/legal/epl-v10.html

 Contributors:
 IBM Corporation - Initial implementation
 **********************************************************************/
package org.erlide.ui.editors.scratch;

import org.eclipse.core.filebuffers.IDocumentSetupParticipant;
import org.eclipse.jface.text.IDocument;

/**
 * The document setup participant for the Java Scratch Editor
 */
public class ScratchDocumentSetupParticipant implements
		IDocumentSetupParticipant {

	public ScratchDocumentSetupParticipant() {
	}

	/*
	 * @see org.eclipse.core.filebuffers.IDocumentSetupParticipant#setup(org.eclipse.jface.text.IDocument)
	 */
	public void setup(IDocument document) {
		if (document != null) {
			// JavaTextTools tools=
			// ErlangPlugin.getDefault().getJavaTextTools();
			// IDocumentPartitioner partitioner=
			// tools.createDocumentPartitioner();
			// partitioner.connect(document);
			// document.setDocumentPartitioner(partitioner);
		}
	}
}
