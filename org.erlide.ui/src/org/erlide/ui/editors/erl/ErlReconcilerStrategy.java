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

// import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.reconciler.DirtyRegion;
import org.eclipse.jface.text.reconciler.IReconcilingStrategy;
import org.eclipse.jface.text.reconciler.IReconcilingStrategyExtension;
import org.erlide.basiccore.ErlLogger;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.IErlModule;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.ui.util.ErlModelUtils;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlReconcilerStrategy implements IReconcilingStrategy,
		IReconcilingStrategyExtension {

	private IErlModule fModule;

	private final ErlangEditor fEditor;

	private IDocument fDoc;

	private IProgressMonitor mon;

	// private boolean initialInsert;

	public ErlReconcilerStrategy(ErlangEditor editor) {
		fEditor = editor;
	}

	public void setDocument(IDocument document) {
		if (fEditor == null) {
			return;
		}
		fDoc = document;
	}

	public void reconcile(DirtyRegion dirtyRegion, IRegion subRegion) {
		ErlLogger.debug("## reconcile " + dirtyRegion.getOffset() + "-"
				+ dirtyRegion.getLength() + " : " + dirtyRegion.getType());
		notify(mkReconcileMsg("reconcile", dirtyRegion, subRegion));
		reconcileModel(fDoc, dirtyRegion);
	}

	private OtpErlangObject mkReconcileMsg(String string,
			DirtyRegion dirtyRegion, IRegion subRegion) {
		final OtpErlangAtom cmd = new OtpErlangAtom(string);
		return cmd;
	}

	public void reconcile(IRegion partition) {
	}

	private void reconcileModel(IDocument doc, DirtyRegion dirtyRegion) {
		Assert.isNotNull(fModule);
		if (dirtyRegion.getType() == DirtyRegion.INSERT) {
			fModule.insertText(dirtyRegion.getOffset(), dirtyRegion.getText());
		} else if (dirtyRegion.getType() == DirtyRegion.REMOVE) {
			fModule
					.removeText(dirtyRegion.getOffset(), dirtyRegion
							.getLength());
		}
		try {
			fModule.open(null);
		} catch (final ErlModelException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		// fModule.reconcile(fDoc, dirtyRegion);
		mon.done();
	}

	public void initialReconcile() {
		// ErlLogger.debug("## initial reconcile ");
		// initialInsert = true;
		fModule = ErlModelUtils.getModule(fEditor);
		Assert.isNotNull(fModule);
		// if (fModule != null) {
		// fModule.getScanner();
		// ErlLogger.debug("## module:: " + fModule.getElementName());
		// }
		notify(new OtpErlangAtom("initialReconcile"));
	}

	private void notify(OtpErlangObject msg) {
		BackendManager.getDefault().getIdeBackend().send("erlide_code_db", msg);
	}

	public void setProgressMonitor(IProgressMonitor monitor) {
		mon = monitor;
	}

}
