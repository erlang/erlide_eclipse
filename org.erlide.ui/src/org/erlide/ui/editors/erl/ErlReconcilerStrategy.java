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
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.reconciler.DirtyRegion;
import org.eclipse.jface.text.reconciler.IReconcilingStrategy;
import org.eclipse.jface.text.reconciler.IReconcilingStrategyExtension;
import org.erlide.basiccore.ErlLogger;
import org.erlide.core.erlang.IErlModule;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.ui.util.ErlModelUtils;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;

public class ErlReconcilerStrategy implements IReconcilingStrategy,
		IReconcilingStrategyExtension {

	private IErlModule fModule;

	private ErlangEditor fEditor;

	private IDocument fDoc;

	private IProgressMonitor mon;

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
		ErlLogger.log("## reconcile " + dirtyRegion.getOffset() + "-"
				+ dirtyRegion.getLength() + " : " + dirtyRegion.getType());

		notify(mkReconcileMsg("reconcile", dirtyRegion, subRegion));
		reconcileModel(fDoc, dirtyRegion);
	}

	private OtpErlangObject mkReconcileMsg(String string,
			DirtyRegion dirtyRegion, IRegion subRegion) {
		OtpErlangAtom cmd = new OtpErlangAtom(string);
		return cmd;
	}

	public void reconcile(IRegion partition) {
	}

	private void reconcileModel(IDocument doc, DirtyRegion dirtyRegion) {
		if (fModule != null) {
			fModule.reconcile(fDoc, dirtyRegion);
		}
	}

	public void initialReconcile() {
		ErlLogger.log("## initial reconcile ");
		fModule = ErlModelUtils.getModule(fEditor);
		if (fModule != null) {
			ErlLogger.log("## module:: " + fModule.getElementName());
		}
		notify(new OtpErlangAtom("initialReconcile"));
	}

	private void notify(OtpErlangObject msg) {
		BackendManager.getDefault().getIdeBackend().send("erlide_code_db", msg);
	}

	public void setProgressMonitor(IProgressMonitor monitor) {
		mon = monitor;
	}

}
