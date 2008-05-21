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
import org.eclipse.jface.text.AbstractInformationControlManager;
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
import org.eclipse.jface.text.information.IInformationPresenter;
import org.eclipse.jface.text.information.IInformationProvider;
import org.eclipse.jface.text.information.InformationPresenter;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.reconciler.IReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.source.IAnnotationHover;
import org.eclipse.jface.text.source.ICharacterPairMatcher;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.editors.text.TextSourceViewerConfiguration;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.core.erlang.IErlModule;
import org.erlide.ui.editors.outline.QuickOutlinePopupDialog;
import org.erlide.ui.editors.util.HTMLTextPresenter;
import org.erlide.ui.util.ErlModelUtils;
import org.erlide.ui.util.IColorManager;

/**
 * The editor configurator
 * 
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class EditorConfiguration extends TextSourceViewerConfiguration {

	final ErlangEditor editor;

	private ITextDoubleClickStrategy doubleClickStrategy;

	// private IErlScanner fScanner;

	private ErlHighlightScanner fHighlightScanner;

	private final IColorManager colorManager;

	private ICharacterPairMatcher fBracketMatcher;

	private InformationPresenter fOutlinePresenter;

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
			final ErlangEditor leditor, final IColorManager lcolorManager) {
		super(store);
		colorManager = lcolorManager;
		editor = leditor;
	}

	/**
	 * The standard content types
	 * 
	 * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getConfiguredContentTypes(org.eclipse.jface.text.source.ISourceViewer)
	 */
	@Override
	public String[] getConfiguredContentTypes(final ISourceViewer sourceViewer) {
		return IErlangPartitions.LEGAL_PARTITIONS;
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

	/**
	 * Creates and returns the fHighlightScanner
	 * 
	 * @return the highlighting fHighlightScanner
	 */
	protected ErlHighlightScanner getHighlightScanner() {
		if (fHighlightScanner == null) {
			fHighlightScanner = new ErlHighlightScanner(colorManager);
		}
		return fHighlightScanner;
	}

	public ICharacterPairMatcher getBracketMatcher() {
		if (fBracketMatcher == null) {
			// TODO: get the ErlPairMatcher to work in some way
			// final IErlScanner scanner = ErlModelUtils.getScanner(editor);
			// fBracketMatcher = new ErlPairMatcher(scanner);
			fBracketMatcher = new ErlJavaPairMatcher(new String[] { "(", ")",
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

		final ErlHighlightScanner scan = getHighlightScanner();
		if (scan != null) {
			final DefaultDamagerRepairer dr = new ErlDamagerRepairer(scan);
			reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
			reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);
		}
		return reconciler;
	}

	/*
	 * @see org.eclipse.jface.text.source.SourceViewerConfiguration#getAutoEditStrategies(org.eclipse.jface.text.source.ISourceViewer,
	 *      java.lang.String)
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
			return new ErlTextHover(module);
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
	public String getConfiguredDocumentPartitioning(
			final ISourceViewer sourceViewer) {
		return IErlangPartitions.ERLANG_PARTITIONING;
	}

	@Override
	public IReconciler getReconciler(final ISourceViewer sourceViewer) {
		final ErlReconcilerStrategy strategy = new ErlReconcilerStrategy(editor);
		final ErlReconciler reconciler = new ErlReconciler(strategy, true);
		// reconciler.setIsIncrementalReconciler(false);
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
					sourceViewer, ""), IDocument.DEFAULT_CONTENT_TYPE);

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

	@Override
	public IAnnotationHover getAnnotationHover(final ISourceViewer sourceViewer) {
		return new ErlangAnnotationHover();
	}

	/*
	 * @see SourceViewerConfiguration#getInformationControlCreator(ISourceViewer)
	 * @since 2.0
	 */
	@Override
	public IInformationControlCreator getInformationControlCreator(
			final ISourceViewer sourceViewer) {
		return new IInformationControlCreator() {

			public IInformationControl createInformationControl(
					final Shell parent) {
				return new DefaultInformationControl(parent, SWT.NONE,
						new HTMLTextPresenter(true));
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
		ourDetectors[ourDetectors.length - 1] = new ErlangHyperlinkDetector(
				editor);

		return ourDetectors;
	}

	/**
	 * Returns the information presenter control creator. The creator is a
	 * factory creating the presenter controls for the given source viewer. This
	 * implementation always returns a creator for
	 * <code>DefaultInformationControl</code> instances.
	 * 
	 * @param sourceViewer
	 *            the source viewer to be configured by this configuration
	 * @return an information control creator
	 */
	/* NOT USED */
	/*
	 * private IInformationControlCreator getInformationPresenterControlCreator(
	 * ISourceViewer sourceViewer) { return new IInformationControlCreator() {
	 * 
	 * public IInformationControl createInformationControl(Shell parent) { final
	 * int shellStyle = SWT.RESIZE | SWT.TOOL; final int style = SWT.V_SCROLL |
	 * SWT.H_SCROLL; return new DefaultInformationControl(parent, shellStyle,
	 * style, new HTMLTextPresenter(false)); } }; }
	 */
	private IInformationControlCreator getOutlinePresenterControlCreator(
			final ISourceViewer sourceViewer, final String commandId) {
		return new IInformationControlCreator() {
			public IInformationControl createInformationControl(
					final Shell parent) {
				final int shellStyle = SWT.RESIZE;
				final QuickOutlinePopupDialog dialog = new QuickOutlinePopupDialog(
						parent, shellStyle, editor, editor);
				return dialog;
			}
		};
	}

	public IInformationPresenter getOutlinePresenter(
			final ISourceViewer sourceViewer) {
		// Ensure the source page is defined
		if (editor == null) {
			return null;
		}
		// Reuse the old outline presenter
		if (fOutlinePresenter != null) {
			return fOutlinePresenter;
		}
		// Define a new outline presenter
		fOutlinePresenter = new InformationPresenter(
				getOutlinePresenterControlCreator(sourceViewer,
						IErlangEditorActionDefinitionIds.SHOW_OUTLINE));
		fOutlinePresenter
				.setDocumentPartitioning(getConfiguredDocumentPartitioning(sourceViewer));
		fOutlinePresenter
				.setAnchor(AbstractInformationControlManager.ANCHOR_GLOBAL);
		// Define a new outline provider
		final IInformationProvider provider = new ErlangSourceInfoProvider(
				editor);
		// Set the provider on all defined content types
		final String[] contentTypes = getConfiguredContentTypes(sourceViewer);
		for (int i = 0; i < contentTypes.length; i++) {
			fOutlinePresenter.setInformationProvider(provider, contentTypes[i]);
		}
		// Set the presenter size constraints
		fOutlinePresenter.setSizeConstraints(50, 20, true, false);

		return fOutlinePresenter;
	}

}
