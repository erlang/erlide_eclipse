package org.erlide.test_support.ui.trace;

import java.util.Map;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ui.editors.text.TextSourceViewerConfiguration;

public class TraceEditorConfiguration extends TextSourceViewerConfiguration {

    public TraceEditorConfiguration(final IPreferenceStore preferenceStore,
            final TraceEditor bterlTraceEditor) {
        super(preferenceStore);
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    @Override
    protected Map getHyperlinkDetectorTargets(final ISourceViewer sourceViewer) {
        final Map res = super.getHyperlinkDetectorTargets(sourceViewer);
        res.put("org.erlide.test_support.trace.target", null);
        return res;
    }

}
