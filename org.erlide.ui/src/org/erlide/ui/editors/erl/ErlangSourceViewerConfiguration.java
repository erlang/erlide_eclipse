package org.erlide.ui.editors.erl;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ui.editors.text.TextSourceViewerConfiguration;
import org.erlide.ui.util.IColorManager;

public class ErlangSourceViewerConfiguration extends
        TextSourceViewerConfiguration {

    protected ErlHighlightScanner fHighlightScanner;
    protected final IColorManager colorManager;

    public ErlangSourceViewerConfiguration(final IPreferenceStore store,
            final IColorManager colorManager) {
        super(store);
        this.colorManager = colorManager;
    }

    /**
     * Creates and returns the fHighlightScanner
     * 
     * @return the highlighting fHighlightScanner
     */
    protected ErlHighlightScanner getHighlightScanner(
            final ISourceViewer sourceViewer) {
        if (fHighlightScanner == null) {
            fHighlightScanner = new ErlHighlightScanner(colorManager,
                    sourceViewer, true);
        }
        return fHighlightScanner;
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

    @Override
    public String getConfiguredDocumentPartitioning(
            final ISourceViewer sourceViewer) {
        return IErlangPartitions.ERLANG_PARTITIONING;
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

        final ErlHighlightScanner scan = getHighlightScanner(sourceViewer);
        if (scan != null) {
            final DefaultDamagerRepairer dr = new ErlDamagerRepairer(scan);
            reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
            reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);
        }
        return reconciler;
    }

}
