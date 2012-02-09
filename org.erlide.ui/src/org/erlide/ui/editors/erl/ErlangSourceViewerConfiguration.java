package org.erlide.ui.editors.erl;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.jface.text.source.ICharacterPairMatcher;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ui.editors.text.TextSourceViewerConfiguration;
import org.erlide.ui.editors.erl.scanner.ErlCodeScanner;
import org.erlide.ui.editors.erl.scanner.ErlCommentScanner;
import org.erlide.ui.editors.erl.scanner.ErlDamagerRepairer;
import org.erlide.ui.editors.erl.scanner.ErlStringScanner;
import org.erlide.ui.editors.erl.scanner.IErlangPartitions;
import org.erlide.ui.prefs.TokenHighlight;
import org.erlide.ui.util.IColorManager;
import org.erlide.ui.util.text.SingleTokenScanner;

public class ErlangSourceViewerConfiguration extends
        TextSourceViewerConfiguration {

    protected final IColorManager colorManager;
    protected ITokenScanner charScanner;
    protected ITokenScanner codeScanner;
    protected final ITokenScanner commentScanner;
    protected final ITokenScanner stringScanner;
    protected final ITokenScanner qatomScanner;
    private ICharacterPairMatcher fBracketMatcher;

    public ErlangSourceViewerConfiguration(final IPreferenceStore store,
            final IColorManager colorManager) {
        super(store);
        this.colorManager = colorManager;
        codeScanner = new ErlCodeScanner(colorManager);
        commentScanner = new ErlCommentScanner(colorManager);
        stringScanner = new ErlStringScanner(colorManager);
        qatomScanner = new SingleTokenScanner(colorManager,
                ErlCodeScanner.getToken(TokenHighlight.ATOM.getName()));
        charScanner = new SingleTokenScanner(colorManager,
                ErlCodeScanner.getToken(TokenHighlight.CHAR.getName()));
    }

    @Override
    public String[] getConfiguredContentTypes(final ISourceViewer sourceViewer) {
        return IErlangPartitions.LEGAL_PARTITIONS;
    }

    @Override
    public String getConfiguredDocumentPartitioning(
            final ISourceViewer sourceViewer) {
        return IErlangPartitions.ERLANG_PARTITIONING;
    }

    @Override
    public IPresentationReconciler getPresentationReconciler(
            final ISourceViewer sourceViewer) {
        final PresentationReconciler reconciler = new ErlangPresentationReconciler();
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

    public ICharacterPairMatcher getBracketMatcher() {
        if (fBracketMatcher == null) {
            fBracketMatcher = new ErlangPairMatcher(new String[] { "(", ")",
                    "{", "}", "[", "]", "<<", ">>" });
        }
        return fBracketMatcher;
    }

}
