/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.editors.erl;

import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.IAutoEditStrategy;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.ITextHover;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.hyperlink.IHyperlinkDetector;
import org.eclipse.jface.text.quickassist.IQuickAssistAssistant;
import org.eclipse.jface.text.quickassist.QuickAssistAssistant;
import org.eclipse.jface.text.reconciler.IReconciler;
import org.eclipse.jface.text.source.ICharacterPairMatcher;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditorPreferenceConstants;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.core.erlang.IErlModule;
import org.erlide.ui.editors.erl.autoedit.AutoIndentStrategy;
import org.erlide.ui.editors.erl.completion.ErlContentAssistProcessor;
import org.erlide.ui.editors.erl.correction.ErlangQuickAssistProcessor;
import org.erlide.ui.editors.erl.hover.ErlTextHover;
import org.erlide.ui.editors.erl.hyperlink.ErlangHyperlinkDetector;
import org.erlide.ui.editors.internal.reconciling.ErlReconciler;
import org.erlide.ui.editors.internal.reconciling.ErlReconcilerStrategy;
import org.erlide.ui.information.ErlInformationPresenter;
import org.erlide.ui.information.PresenterControlCreator;
import org.erlide.ui.util.ErlModelUtils;
import org.erlide.ui.util.IColorManager;
import org.erlide.ui.util.eclipse.BrowserInformationControl;

/**
 * The editor configurator
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class EditorConfiguration extends ErlangSourceViewerConfiguration {

	final ErlangEditor editor;
	private ITextDoubleClickStrategy doubleClickStrategy;
	private ICharacterPairMatcher fBracketMatcher;
	private ErlReconciler reconciler;

	/**
	 * Default configuration constructor
	 * 
	 * @param store
	 * 
	 * @param editor
	 * 
	 * @param lcolorManager
	 *            the color manager
	 */
	public EditorConfiguration(final IPreferenceStore store,
			final ErlangEditor leditor, final IColorManager colorManager) {
		super(store, colorManager);
		editor = leditor;
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

	/*
	 * @see
	 * org.eclipse.jface.text.source.SourceViewerConfiguration#getAutoEditStrategies
	 * (org.eclipse.jface.text.source.ISourceViewer, java.lang.String)
	 */
	@Override
	public IAutoEditStrategy[] getAutoEditStrategies(
			final ISourceViewer sourceViewer, final String contentType) {
		// final String partitioning =
		// getConfiguredDocumentPartitioning(sourceViewer);
		return new IAutoEditStrategy[] { new AutoIndentStrategy(editor) };
	}

	@Override
	public ITextHover getTextHover(final ISourceViewer sourceViewer,
			final String contentType) {
		final IErlModule module = ErlModelUtils.getModule(editor);
		if (module != null) {
			return new ErlTextHover(editor, module);
		}
		return null;
	}

	/**
	 * Returns the editor in which the configured viewer(s) will reside.
	 * 
	 * @return the enclosing editor
	 */
	protected ITextEditor getEditor() {
		return editor;
	}

	@Override
	public IReconciler getReconciler(final ISourceViewer sourceViewer) {
		final ErlReconcilerStrategy strategy = new ErlReconcilerStrategy(editor);
		reconciler = new ErlReconciler(strategy, true, true);
		reconciler.setProgressMonitor(new NullProgressMonitor());
		reconciler.setDelay(500);
		return reconciler;
	}

	@Override
	public IContentAssistant getContentAssistant(
			final ISourceViewer sourceViewer) {
		if (getEditor() != null) {
			final ContentAssistant asst = new ContentAssistant();

			asst.setContentAssistProcessor(new ErlContentAssistProcessor(
					sourceViewer, editor.getModule()),
					IDocument.DEFAULT_CONTENT_TYPE);

			asst.enableAutoActivation(true);
			asst.setAutoActivationDelay(500);
			asst.enableAutoInsert(true);
			asst.enablePrefixCompletion(false);
			asst.setDocumentPartitioning(IErlangPartitions.ERLANG_PARTITIONING);

			asst
					.setProposalPopupOrientation(IContentAssistant.PROPOSAL_OVERLAY);
			asst
					.setContextInformationPopupOrientation(IContentAssistant.CONTEXT_INFO_ABOVE);
			asst
					.setInformationControlCreator(getInformationControlCreator(sourceViewer));

			return asst;
		}
		return null;
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
				if (parent.getText().length() == 0
						&& BrowserInformationControl.isAvailable(parent)) {
					final BrowserInformationControl info = new BrowserInformationControl(
							parent, JFaceResources.DIALOG_FONT, EditorsUI
									.getTooltipAffordanceString()) {
						@Override
						public IInformationControlCreator getInformationPresenterControlCreator() {
							return new PresenterControlCreator();
						}
					};
					return info;
				} else {
					return new DefaultInformationControl(parent, EditorsUI
							.getTooltipAffordanceString(),
							new ErlInformationPresenter(true));
				}
			}
		};
	}

	@Override
	public IHyperlinkDetector[] getHyperlinkDetectors(
			final ISourceViewer sourceViewer) {
		if (editor == null) {
			return null;
		}

		// Add ErlangSubHyperlinkDetector to the list provided by the superclass

		IHyperlinkDetector[] superDetectors = super
				.getHyperlinkDetectors(sourceViewer);
		if (superDetectors == null) {
			superDetectors = new IHyperlinkDetector[0];
		}
		final IHyperlinkDetector[] ourDetectors = new IHyperlinkDetector[superDetectors.length + 1];
		for (int i = 0; i < superDetectors.length; ++i) {
			ourDetectors[i] = superDetectors[i];
		}
		ourDetectors[ourDetectors.length - 1] = new ErlangHyperlinkDetector(
				editor);

		return ourDetectors;
	}

	public void resetReconciler() {
		if (reconciler != null) {
			reconciler.reset();
		}
	}

	public void reconcileNow() {
		if (reconciler != null) {
			reconciler.reconcileNow();
		}
	}

	@Override
	public IQuickAssistAssistant getQuickAssistAssistant(
			final ISourceViewer sourceViewer) {
		final IQuickAssistAssistant assistant = new QuickAssistAssistant();
		assistant.setQuickAssistProcessor(new ErlangQuickAssistProcessor());
		assistant
				.setInformationControlCreator(getQuickAssistAssistantInformationControlCreator());
		return assistant;
	}

	/**
	 * Returns the information control creator for the quick assist assistant.
	 * 
	 * @return the information control creator
	 * @since 3.3
	 */
	private IInformationControlCreator getQuickAssistAssistantInformationControlCreator() {
		return new IInformationControlCreator() {
			public IInformationControl createInformationControl(
					final Shell parent) {
				final String affordance = getAdditionalInfoAffordanceString();
				return new DefaultInformationControl(parent, affordance);
			}
		};
	}

	static final String getAdditionalInfoAffordanceString() {
		if (!EditorsUI
				.getPreferenceStore()
				.getBoolean(
						AbstractDecoratedTextEditorPreferenceConstants.EDITOR_SHOW_TEXT_HOVER_AFFORDANCE)) {
			return null;
		}

		return "Press 'Tab' from proposal table or click for focus";
	}

}
