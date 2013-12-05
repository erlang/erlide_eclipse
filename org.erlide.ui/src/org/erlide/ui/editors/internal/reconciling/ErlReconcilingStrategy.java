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
package org.erlide.ui.editors.internal.reconciling;

// import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.reconciler.DirtyRegion;
import org.eclipse.jface.text.reconciler.IReconcilingStrategyExtension;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.services.parsing.ScannerService;
import org.erlide.ui.editors.erl.AbstractErlangEditor;
import org.erlide.util.ErlLogger;

public class ErlReconcilingStrategy implements IErlReconcilingStrategy,
        IReconcilingStrategyExtension {

    private IErlModule fModule;
    private final AbstractErlangEditor fEditor;
    // private IDocument fDoc;
    private IProgressMonitor mon;
    private ScannerService fScanner;

    // private boolean initialInsert;

    public ErlReconcilingStrategy(final AbstractErlangEditor editor) {
        fEditor = editor;
    }

    @Override
    public void setDocument(final IDocument document) {
        if (fEditor == null) {
            return;
        }
        // fDoc = document;
    }

    @Override
    public void reconcile(final DirtyRegion dirtyRegion, final IRegion subRegion) {
        ErlLogger.error("reconcile called");
    }

    @Override
    public void reconcile(final IRegion partition) {
        ErlLogger.error("reconcile called");
    }

    @Override
    public void initialReconcile() {
        fModule = fEditor != null ? fEditor.getModule() : null;
        fScanner = fEditor != null ? fEditor.getScanner() : null;
        if (fModule != null) {
            fModule.initialReconcile();
        }
        // notify(new OtpErlangAtom("initialReconcile"));
    }

    @Override
    public void setProgressMonitor(final IProgressMonitor monitor) {
        mon = monitor;
    }

    @Override
    public void uninstall() {
        if (fModule != null) {
            fModule.finalReconcile();
        }
    }

    @Override
    public void chunkReconciled() {
        if (fModule != null) {
            fModule.postReconcile(mon);
        }
    }

    @Override
    public void reconcile(final ErlDirtyRegion r) {
        if (fModule != null) {
            fModule.reconcileText(r.getOffset(), r.getLength(), r.getText(), mon);
        } else if (fScanner != null) {
            fScanner.replaceText(r.getOffset(), r.getLength(), r.getText());
        }
    }

    public IErlModule getModule() {
        return fModule;
    }

}
