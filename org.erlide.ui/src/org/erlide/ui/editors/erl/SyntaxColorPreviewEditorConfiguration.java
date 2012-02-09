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

import java.util.Map;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.IAutoEditStrategy;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.source.ISourceViewer;
import org.erlide.ui.editors.erl.scanner.ErlDamagerRepairer;
import org.erlide.ui.editors.erl.scanner.IErlangPartitions;
import org.erlide.ui.prefs.HighlightStyle;
import org.erlide.ui.prefs.SyntaxColorPreviewHighlightScanner;
import org.erlide.ui.prefs.TokenHighlight;
import org.erlide.ui.util.IColorManager;

public class SyntaxColorPreviewEditorConfiguration extends
        ErlangSourceViewerConfiguration {

    public SyntaxColorPreviewEditorConfiguration(final IPreferenceStore store,
            final IColorManager lcolorManager,
            final Map<TokenHighlight, HighlightStyle> styles) {
        super(store, lcolorManager);
        codeScanner = new SyntaxColorPreviewHighlightScanner(colorManager,
                styles);
    }

    @Override
    public IPresentationReconciler getPresentationReconciler(
            final ISourceViewer sourceViewer) {
        final PresentationReconciler reconciler = new PresentationReconciler();
        reconciler
                .setDocumentPartitioning(getConfiguredDocumentPartitioning(sourceViewer));
        DefaultDamagerRepairer dr = new ErlDamagerRepairer(codeScanner);
        reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
        reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);
        dr = new ErlDamagerRepairer(commentScanner);
        reconciler.setDamager(dr, IErlangPartitions.ERLANG_COMMENT);
        reconciler.setRepairer(dr, IErlangPartitions.ERLANG_COMMENT);
        dr = new ErlDamagerRepairer(stringScanner);
        reconciler.setDamager(dr, IErlangPartitions.ERLANG_STRING);
        reconciler.setRepairer(dr, IErlangPartitions.ERLANG_STRING);
        dr = new ErlDamagerRepairer(qatomScanner);
        reconciler.setDamager(dr, IErlangPartitions.ERLANG_QATOM);
        reconciler.setRepairer(dr, IErlangPartitions.ERLANG_QATOM);
        dr = new ErlDamagerRepairer(charScanner);
        reconciler.setDamager(dr, IErlangPartitions.ERLANG_CHARACTER);
        reconciler.setRepairer(dr, IErlangPartitions.ERLANG_CHARACTER);

        return reconciler;
    }

    @Override
    public ITextDoubleClickStrategy getDoubleClickStrategy(
            final ISourceViewer sourceViewer, final String contentType) {
        return null;
    }

    public IAutoEditStrategy getAutoEditStrategy(
            final ISourceViewer sourceViewer, final String contentType) {
        return null;
    }

    @Override
    public ITextHover getTextHover(final ISourceViewer sourceViewer,
            final String contentType) {
        return null;
    }

}
