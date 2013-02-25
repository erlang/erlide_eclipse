package org.erlide.ui.editors.erl;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewerExtension;
import org.eclipse.jface.text.source.ICharacterPairMatcher;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.texteditor.ITextEditorExtension3;
import org.eclipse.ui.texteditor.SourceViewerDecorationSupport;
import org.erlide.model.erlang.ErlToken;
import org.erlide.model.erlang.IErlModule;
import org.erlide.model.erlang.IErlScanner;
import org.erlide.model.root.IErlElement;
import org.erlide.model.root.IErlProject;
import org.erlide.ui.prefs.PreferenceConstants;

public abstract class AbstractErlangEditor extends TextEditor {

    /** Preference key for matching brackets */
    protected final static String MATCHING_BRACKETS = PreferenceConstants.EDITOR_MATCHING_BRACKETS;
    /** Preference key for matching brackets color */
    protected final static String MATCHING_BRACKETS_COLOR = PreferenceConstants.EDITOR_MATCHING_BRACKETS_COLOR;
    /** The bracket inserter. */
    private ErlangViewerBracketInserter fBracketInserter = null;

    public abstract void reconcileNow();

    public abstract IErlElement getElementAt(int offset, boolean b);

    public abstract ErlToken getTokenAt(int offset);

    public abstract IErlModule getModule();

    public abstract IDocument getDocument();

    public abstract IErlScanner getScanner();

    @Override
    protected void configureSourceViewerDecorationSupport(
            final SourceViewerDecorationSupport support) {
        support.setCharacterPairMatcher(getBracketMatcher());
        support.setMatchingCharacterPainterPreferenceKeys(MATCHING_BRACKETS,
                MATCHING_BRACKETS_COLOR);

        super.configureSourceViewerDecorationSupport(support);
    }

    public ICharacterPairMatcher getBracketMatcher() {
        return ((ErlangSourceViewerConfiguration) getSourceViewerConfiguration())
                .getBracketMatcher();
    }

    protected ErlangViewerBracketInserter getBracketInserter() {
        if (fBracketInserter == null) {
            fBracketInserter = new ErlangViewerBracketInserter(
                    getSourceViewer());
        }
        return fBracketInserter;
    }

    public abstract IErlProject getProject();

    public abstract String getScannerName();

    protected void setupBracketInserter() {
        final ISourceViewer sourceViewer = getSourceViewer();
        if (sourceViewer instanceof ITextViewerExtension) {
            ((ITextViewerExtension) sourceViewer)
                    .prependVerifyKeyListener(getBracketInserter());
        }
    }

    @Override
    protected ISourceViewer createSourceViewer(final Composite parent,
            final IVerticalRuler ruler, final int styles) {
        final ISourceViewer viewer = new ErlangSourceViewer(parent, ruler,
                getOverviewRuler(), true, styles,
                new IBracketInserterValidator() {
                    @Override
                    public boolean earlyCancelCheck() {
                        return getInsertMode() != ITextEditorExtension3.SMART_INSERT;
                    }

                    @Override
                    public boolean validInput() {
                        return validateEditorInputState();
                    }
                });
        getSourceViewerDecorationSupport(viewer);

        addFoldingSupport(viewer);

        return viewer;
    }

    protected abstract void addFoldingSupport(final ISourceViewer viewer);

}
