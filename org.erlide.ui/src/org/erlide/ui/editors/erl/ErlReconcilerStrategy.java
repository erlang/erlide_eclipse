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
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModule;
import org.erlide.jinterface.ErlUtils;
import org.erlide.jinterface.ParserException;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.runtime.ErlLogger;
import org.erlide.ui.util.ErlModelUtils;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

public class ErlReconcilerStrategy implements IReconcilingStrategy,
		IReconcilingStrategyExtension {

	private IErlModule fModule;
	private final ErlangEditor fEditor;
	private IDocument fDoc;
	private IProgressMonitor mon;

	// private boolean initialInsert;

	public ErlReconcilerStrategy(final ErlangEditor editor) {
		fEditor = editor;
	}

	public void setDocument(final IDocument document) {
		if (fEditor == null) {
			return;
		}
		fDoc = document;
	}

	public void reconcile(final DirtyRegion dirtyRegion, final IRegion subRegion) {
		// ErlLogger.debug("## reconcile " + fModule + ": " + dirtyRegion + " "
		// + subRegion);
		notify(mkReconcileMsg("reconcile", dirtyRegion, subRegion));
		reconcileModel(fDoc, dirtyRegion);
	}

	@SuppressWarnings("boxing")
	private OtpErlangObject mkReconcileMsg(final String string,
			final DirtyRegion dirtyRegion, final IRegion subRegion) {
		OtpErlangObject msg = new OtpErlangString(
				"reconcile (message build failed)");
		try {
			String text = dirtyRegion.getText();
			if (text == null) {
				text = "";
			}
			msg = ErlUtils.format("{~a, {~i, ~i, ~a, ~b}, {~i, ~i}}", string,
					dirtyRegion.getOffset(), dirtyRegion.getLength(),
					dirtyRegion.getType(), text, subRegion.getOffset(),
					subRegion.getLength());
		} catch (ParserException e) {
			e.printStackTrace();
		} catch (RpcException e) {
			e.printStackTrace();
		}
		return msg;
	}

	public void reconcile(final IRegion partition) {
		// ErlLogger.debug("## reconcile " + fModule + ": " + partition);
		reconcile(new DirtyRegion(partition.getOffset(), partition.getLength(),
				DirtyRegion.INSERT, fDoc.get()), partition);
	}

	private void reconcileModel(final IDocument doc,
			final DirtyRegion dirtyRegion) {
		if (fModule != null) {
			// ErlLogger.debug("## reconcile " + fModule.getName() + " "
			// + dirtyRegion.getOffset() + "-" + dirtyRegion.getLength()
			// + " : " + dirtyRegion.getType());
			if (dirtyRegion.getType() == DirtyRegion.INSERT) {
				fModule.reconcileText(dirtyRegion.getOffset(), 0, dirtyRegion
						.getText(), mon);
			} else if (dirtyRegion.getType() == DirtyRegion.REMOVE) {
				fModule.reconcileText(dirtyRegion.getOffset(), dirtyRegion
						.getLength(), "", mon);
			}
		}
		mon.done();
	}

	public void initialReconcile() {
		// initialInsert = true;
		fModule = ErlModelUtils.getModule(fEditor);
		ErlLogger.debug("## initial reconcile "
				+ (fModule != null ? fModule.getName() : ""));
		// Assert.isNotNull(fModule);
		// if (fModule != null) {
		// fModule.getScanner();
		// ErlLogger.debug("## module:: " + fModule.getElementName());
		// }
		if (fModule != null) {
			fModule.initialReconcile();
		}
		notify(new OtpErlangAtom("initialReconcile"));
	}

	private void notify(final OtpErlangObject msg) {
		if (System.getProperty("erlide.reconcile.debug") != null) {
			ErlLogger.debug("RECONCILE %s", msg.toString());
		}
		ErlangCore.getBackendManager().getIdeBackend().send("erlide_code_db",
				msg);
	}

	public void setProgressMonitor(final IProgressMonitor monitor) {
		mon = monitor;
	}

	public void uninstall() {
		if (fModule != null) {
			fModule.finalReconcile();
		}
	}

}
