/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.console;

import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.jface.text.source.ICharacterPairMatcher;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.editors.text.TextSourceViewerConfiguration;
import org.erlide.backend.console.IoRequest.IoRequestKind;
import org.erlide.ui.editors.erl.ColorManager;
import org.erlide.ui.editors.erl.DoubleClickStrategy;
import org.erlide.ui.editors.erl.ErlangPairMatcher;
import org.erlide.ui.editors.erl.completion.ErlContentAssistProcessor;
import org.erlide.ui.editors.erl.scanner.ErlCodeScanner;
import org.erlide.ui.editors.erl.scanner.ErlDamagerRepairer;
import org.erlide.ui.internal.information.ErlInformationPresenter;

final public class ErlangConsoleSourceViewerConfiguration extends
        TextSourceViewerConfiguration {

    public ErlangConsoleSourceViewerConfiguration() {
        super();
    }

    private DoubleClickStrategy doubleClickStrategy;
    private ICharacterPairMatcher fBracketMatcher;

    @Override
    public IContentAssistant getContentAssistant(
            final ISourceViewer sourceViewer) {

        final ContentAssistant contentAssistant = new ContentAssistant();

        // TODO vi vill ha in en punkt h�r, men den f�r return till
        // styledtext o skickar allt f�r tidigt...
        final ErlContentAssistProcessor contentAssistProcessor = new ErlContentAssistProcessor(
                sourceViewer, null, contentAssistant);
        contentAssistant.setContentAssistProcessor(contentAssistProcessor,
                IDocument.DEFAULT_CONTENT_TYPE);
        contentAssistProcessor.setToPrefs();
        contentAssistant.enableAutoInsert(true);
        contentAssistant.enablePrefixCompletion(false);
        // asst.setDocumentPartitioning(IErlangPartitions.ERLANG_PARTITIONING
        // );

        contentAssistant
                .setProposalPopupOrientation(IContentAssistant.PROPOSAL_OVERLAY);
        contentAssistant
                .setContextInformationPopupOrientation(IContentAssistant.CONTEXT_INFO_ABOVE);
        contentAssistant
                .setInformationControlCreator(getInformationControlCreator(sourceViewer));

        return contentAssistant;
    }

    /*
     * @see
     * SourceViewerConfiguration#getInformationControlCreator(ISourceViewer)
     * 
     * @since 2.0
     */
    @Override
    public IInformationControlCreator getInformationControlCreator(
            final ISourceViewer sourceViewer) {
        return new IInformationControlCreator() {

            @Override
            public IInformationControl createInformationControl(
                    final Shell parent) {
                return new DefaultInformationControl(parent,
                        new ErlInformationPresenter(true));
            }
        };
    }

    /**
     * The double click strategy
     * 
     * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getDoubleClickStrategy(org.eclipse.jface.text.source.ISourceViewer,
     *      java.lang.String)
     */
    @Override
    public ITextDoubleClickStrategy getDoubleClickStrategy(
            final ISourceViewer sourceViewer, final String contentType) {
        if (doubleClickStrategy == null) {
            // doubleClickStrategy = new
            // ErlDoubleClickSelector(getBracketMatcher());
            doubleClickStrategy = new DoubleClickStrategy(getBracketMatcher());
        }
        return doubleClickStrategy;
    }

    public ICharacterPairMatcher getBracketMatcher() {
        if (fBracketMatcher == null) {
            fBracketMatcher = new ErlangPairMatcher(new String[] { "(", ")",
                    "{", "}", "[", "]", "<<", ">>" });
        }
        return fBracketMatcher;
    }

    /**
     * Creates the reconciler
     * 
     * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getPresentationReconciler(org.eclipse.jface.text.source.ISourceViewer)
     */
    @Override
    public IPresentationReconciler getPresentationReconciler(
            final ISourceViewer sourceViewer) {
        final PresentationReconciler reconciler = new PresentationReconciler();
        DefaultDamagerRepairer dr;

        final ColorManager colorManager = new ColorManager();
        final ITokenScanner scan = new ErlCodeScanner(colorManager);
        dr = new ErlDamagerRepairer(scan);
        reconciler.setDamager(dr, IoRequestKind.INPUT.name());
        reconciler.setRepairer(dr, IoRequestKind.INPUT.name());

        final ITokenScanner scan3 = new ConsoleOutputScanner(colorManager);
        dr = new ErlDamagerRepairer(scan3);
        reconciler.setDamager(dr, IoRequestKind.OUTPUT.name());
        reconciler.setRepairer(dr, IoRequestKind.OUTPUT.name());

        reconciler.setDamager(dr, IoRequestKind.PROMPT.name());
        reconciler.setRepairer(dr, IoRequestKind.PROMPT.name());

        reconciler.setDamager(dr, IoRequestKind.STDOUT.name());
        reconciler.setRepairer(dr, IoRequestKind.STDOUT.name());

        reconciler.setDamager(dr, IoRequestKind.STDERR.name());
        reconciler.setRepairer(dr, IoRequestKind.STDERR.name());

        reconciler.setDamager(dr, IoRequestKind.HEADER.name());
        reconciler.setRepairer(dr, IoRequestKind.HEADER.name());

        // this is for the input field
        final ITokenScanner scan2 = new ErlCodeScanner(colorManager);
        dr = new ErlDamagerRepairer(scan2);
        reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
        reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);

        return reconciler;
    }
}
