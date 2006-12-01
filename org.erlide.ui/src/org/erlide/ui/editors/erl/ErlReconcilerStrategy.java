/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl;

//import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.reconciler.DirtyRegion;
import org.eclipse.jface.text.reconciler.IReconcilingStrategy;
import org.erlide.core.erlang.IErlModule;
import org.erlide.ui.util.ErlModelUtils;

public class ErlReconcilerStrategy implements IReconcilingStrategy {

	private IErlModule fModule;

	private ErlangEditor fEditor;

	private IDocument fDoc;

	//private IProgressMonitor mon;

	public ErlReconcilerStrategy(ErlangEditor editor) {
		fEditor = editor;
	}

	public void setDocument(IDocument document) {
		if (fEditor == null) {
			return;
		}
		fDoc = document;

		fModule = ErlModelUtils.getModule(fEditor);
		// System.out.println("set doc:: " + fModule.getElementName());
	}

	public void reconcile(DirtyRegion dirtyRegion, IRegion subRegion) {
		// System.out.println("incremental reconcile " + dirtyRegion.getOffset()
		// + "-"
		// + dirtyRegion.getLength() + ":" + dirtyRegion.getText() + "/"
		// + dirtyRegion.getType());
		reconcileModel(fDoc, dirtyRegion);
	}

	public void reconcile(IRegion partition) {
		if (fEditor == null) {
			return;
		}

		// System.out.println("??? reconcile");
		// reconcileModel(fDoc);
	}

	private void reconcileModel(IDocument doc, DirtyRegion dirtyRegion) {
		if (fModule != null) {
			fModule.reconcile(fDoc, dirtyRegion);
		}
	}

	/* NOT USED */
	/*
	private void reconcileModel(IDocument doc) {
		reconcileModel(doc, new DirtyRegion(0, doc.getLength(),
				DirtyRegion.REMOVE, null));
		reconcileModel(doc, new DirtyRegion(0, doc.getLength(),
				DirtyRegion.INSERT, doc.get()));
	}
	*/

}
