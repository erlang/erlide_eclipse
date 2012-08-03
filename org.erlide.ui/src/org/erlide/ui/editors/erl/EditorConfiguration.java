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

import java.util.Map;

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
import org.eclipse.jface.text.quickassist.IQuickAssistAssistant;
import org.eclipse.jface.text.quickassist.QuickAssistAssistant;
import org.eclipse.jface.text.reconciler.IReconciler;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditorPreferenceConstants;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.ui.editors.erl.autoedit.AutoIndentStrategy;
import org.erlide.ui.editors.erl.completion.ErlContentAssistProcessor;
import org.erlide.ui.editors.erl.completion.ErlStringContentAssistProcessor;
import org.erlide.ui.editors.erl.correction.ErlangQuickAssistProcessor;
import org.erlide.ui.editors.erl.hover.ErlTextHover;
import org.erlide.ui.editors.erl.scanner.IErlangPartitions;
import org.erlide.ui.editors.internal.reconciling.ErlReconciler;
import org.erlide.ui.editors.internal.reconciling.ErlReconcilerStrategy;
import org.erlide.ui.internal.information.ErlInformationPresenter;
import org.erlide.ui.internal.information.PresenterControlCreator;
import org.erlide.ui.util.IColorManager;
import org.erlide.ui.util.eclipse.text.BrowserInformationControl;

/**
 * The editor configurator
 * 
 * @author Eric Merritt [cyberlync at gmail dot com]
 */
public class EditorConfiguration extends ErlangSourceViewerConfiguration {

    final ErlangEditor editor;
    private ITextDoubleClickStrategy doubleClickStrategy;
    private ErlReconciler reconciler;
    private ErlContentAssistProcessor contentAssistProcessor;
    private ErlStringContentAssistProcessor contentAssistProcessorForStrings;
    private final static IAutoEditStrategy[] NO_AUTOEDIT = new IAutoEditStrategy[] {};

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
            final ErlangEditor editor, final IColorManager colorManager) {
        super(store, colorManager);
        this.editor = editor;
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

    @Override
    public IAutoEditStrategy[] getAutoEditStrategies(
            final ISourceViewer sourceViewer, final String contentType) {
        if (contentType.equals(IDocument.DEFAULT_CONTENT_TYPE)) {
            return new IAutoEditStrategy[] { new AutoIndentStrategy(editor) };
        } else {
            return NO_AUTOEDIT;
        }
    }

    @Override
    public ITextHover getTextHover(final ISourceViewer sourceViewer,
            final String contentType) {
        return new ErlTextHover(editor);
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
        final IErlModule module = editor != null ? editor.getModule() : null;
        final String path = module != null ? module.getFilePath() : null;
        reconciler = new ErlReconciler(strategy, true, true, path, module);
        reconciler.setProgressMonitor(new NullProgressMonitor());
        reconciler.setIsAllowedToModifyDocument(false);
        reconciler.setDelay(500);
        return reconciler;
    }

    @Override
    public IContentAssistant getContentAssistant(
            final ISourceViewer sourceViewer) {
        if (editor != null) {
            final ContentAssistant contentAssistant = new ContentAssistant();
            contentAssistant
                    .setDocumentPartitioning(getConfiguredDocumentPartitioning(sourceViewer));

            final IErlModule module = editor.getModule();
            contentAssistProcessor = new ErlContentAssistProcessor(
                    sourceViewer, module, contentAssistant);
            contentAssistProcessorForStrings = new ErlStringContentAssistProcessor(
                    sourceViewer, module, contentAssistant);

            contentAssistProcessor.setToPrefs();
            contentAssistant.setContentAssistProcessor(contentAssistProcessor,
                    IDocument.DEFAULT_CONTENT_TYPE);
            contentAssistant.setContentAssistProcessor(
                    contentAssistProcessorForStrings,
                    IErlangPartitions.ERLANG_STRING);
            contentAssistant.enableAutoInsert(true);
            contentAssistant.enablePrefixCompletion(false);
            contentAssistant
                    .setDocumentPartitioning(IErlangPartitions.ERLANG_PARTITIONING);

            contentAssistant
                    .setProposalPopupOrientation(IContentAssistant.PROPOSAL_OVERLAY);
            contentAssistant
                    .setContextInformationPopupOrientation(IContentAssistant.CONTEXT_INFO_ABOVE);
            contentAssistant
                    .setInformationControlCreator(getInformationControlCreator(sourceViewer));

            return contentAssistant;
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

            @Override
            public IInformationControl createInformationControl(
                    final Shell parent) {
                if (parent.getText().length() == 0
                        && BrowserInformationControl.isAvailable(parent)
                        && editor != null) {
                    final BrowserInformationControl info = new BrowserInformationControl(
                            parent, JFaceResources.DIALOG_FONT,
                            EditorsUI.getTooltipAffordanceString()) {
                        @Override
                        public IInformationControlCreator getInformationPresenterControlCreator() {
                            return new PresenterControlCreator(editor);
                        }
                    };
                    return info;
                } else {
                    return new DefaultInformationControl(parent,
                            EditorsUI.getTooltipAffordanceString(),
                            new ErlInformationPresenter(true));
                }
            }
        };
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    @Override
    protected Map getHyperlinkDetectorTargets(final ISourceViewer sourceViewer) {
        final Map map = super.getHyperlinkDetectorTargets(sourceViewer);
        map.put("org.erlide.ui.hyperlinktarget", getEditor());
        return map;
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
            @Override
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

    public void disposeContentAssistProcessors() {
        contentAssistProcessor.dispose();
        contentAssistProcessor = null;
        contentAssistProcessorForStrings = null;
    }

}
