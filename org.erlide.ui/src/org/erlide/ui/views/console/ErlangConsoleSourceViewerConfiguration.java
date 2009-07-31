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
package org.erlide.ui.views.console;

import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IAutoEditStrategy;
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
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.editors.text.TextSourceViewerConfiguration;
import org.erlide.ui.editors.erl.ColorManager;
import org.erlide.ui.editors.erl.DoubleClickStrategy;
import org.erlide.ui.editors.erl.ErlContentAssistProcessor;
import org.erlide.ui.editors.erl.ErlDamagerRepairer;
import org.erlide.ui.editors.erl.ErlHighlightScanner;
import org.erlide.ui.editors.erl.ErlangPairMatcher;
import org.erlide.ui.util.eclipse.HTMLTextPresenter;

final class ErlangConsoleSourceViewerConfiguration extends
		TextSourceViewerConfiguration {

	ErlangConsoleSourceViewerConfiguration() {
	}

	private DoubleClickStrategy doubleClickStrategy;
	private ICharacterPairMatcher fBracketMatcher;

	@Override
	public IAutoEditStrategy[] getAutoEditStrategies(
			final ISourceViewer sourceViewer, final String contentType) {
		return null;
	}

	@Override
	public IContentAssistant getContentAssistant(
			final ISourceViewer sourceViewer) {

		final ContentAssistant asst = new ContentAssistant();

		// TODO vi vill ha in en punkt h�r, men den f�r return till
		// styledtext o skickar allt f�r tidigt...
		asst.setContentAssistProcessor(new ErlContentAssistProcessor(
				sourceViewer, null), IDocument.DEFAULT_CONTENT_TYPE);

		asst.enableAutoActivation(true);
		asst.setAutoActivationDelay(500);
		asst.enableAutoInsert(true);
		asst.enablePrefixCompletion(false);
		// asst.setDocumentPartitioning(IErlangPartitions.ERLANG_PARTITIONING
		// );

		asst.setProposalPopupOrientation(IContentAssistant.PROPOSAL_OVERLAY);
		asst
				.setContextInformationPopupOrientation(IContentAssistant.CONTEXT_INFO_ABOVE);
		asst
				.setInformationControlCreator(getInformationControlCreator(sourceViewer));

		return asst;
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

			public IInformationControl createInformationControl(
					final Shell parent) {
				return new DefaultInformationControl(parent,
						new HTMLTextPresenter(true));
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

		ColorManager colorManager = new ColorManager();
		final ITokenScanner scan = new ErlHighlightScanner(colorManager,
				sourceViewer, false, new RGB(245, 245, 245));
		if (scan != null) {
			final DefaultDamagerRepairer dr = new ErlDamagerRepairer(scan);
			reconciler.setDamager(dr, ErlConsoleDocument.INPUT_TYPE);
			reconciler.setRepairer(dr, ErlConsoleDocument.INPUT_TYPE);
		}

		// we might want to highlight the output differently (because it might
		// not make sense, but how do we detect it?)
		final ITokenScanner scan3 = new ConsoleOutputScanner(colorManager);
		if (scan3 != null) {
			final DefaultDamagerRepairer dr = new ErlDamagerRepairer(scan3);
			reconciler.setDamager(dr, ErlConsoleDocument.OUTPUT_TYPE);
			reconciler.setRepairer(dr, ErlConsoleDocument.OUTPUT_TYPE);
		}

		// this is for the input field
		final ITokenScanner scan2 = new ErlHighlightScanner(colorManager,
				sourceViewer, false);
		if (scan2 != null) {
			final DefaultDamagerRepairer dr = new ErlDamagerRepairer(scan2);
			reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
			reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);
		}

		return reconciler;
	}
}