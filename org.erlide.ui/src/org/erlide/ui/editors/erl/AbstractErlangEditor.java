package org.erlide.ui.editors.erl;

import java.util.List;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextViewerExtension;
import org.eclipse.jface.text.source.ICharacterPairMatcher;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.texteditor.SourceViewerDecorationSupport;
import org.erlide.model.erlang.ErlToken;
import org.erlide.model.erlang.IErlModule;
import org.erlide.model.erlang.IErlScanner;
import org.erlide.model.root.IErlElement;
import org.erlide.model.root.IErlProject;
import org.erlide.ui.editors.erl.autoedit.SmartTypingPreferencePage;
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
        readBracketInserterPrefs(getBracketInserter());

        final ISourceViewer sourceViewer = getSourceViewer();
        if (sourceViewer instanceof ITextViewerExtension) {
            ((ITextViewerExtension) sourceViewer)
                    .prependVerifyKeyListener(getBracketInserter());
        }
    }

    public static void readBracketInserterPrefs(
            final ErlangViewerBracketInserter bracketInserter) {
        final List<Boolean> prefs = SmartTypingPreferencePage
                .getBracketInserterPreferences();
        bracketInserter.setCloseAtomsEnabled(prefs
                .get(SmartTypingPreferencePage.ATOMS));
        bracketInserter.setCloseBracketsEnabled(prefs
                .get(SmartTypingPreferencePage.BRACKETS));
        bracketInserter.setCloseStringsEnabled(prefs
                .get(SmartTypingPreferencePage.STRINGS));
        bracketInserter.setCloseBracesEnabled(prefs
                .get(SmartTypingPreferencePage.BRACES));
        bracketInserter.setCloseParensEnabled(prefs
                .get(SmartTypingPreferencePage.PARENS));
        bracketInserter.setEmbraceSelectionEnabled(prefs
                .get(SmartTypingPreferencePage.EMBRACE_SELECTION));
    }

}
