package org.erlide.ui.editors.erl;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.source.IOverviewRuler;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.jface.text.source.projection.ProjectionViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.texteditor.ChainedPreferenceStore;
import org.erlide.ui.prefs.PreferenceConstants;
import org.erlide.ui.prefs.TokenHighlight;
import org.erlide.ui.prefs.plugin.internal.ErlangSourceViewerUpdater;
import org.erlide.ui.util.IColorManager;
import org.erlide.util.IDisposable;

public class ErlangSourceViewer extends ProjectionViewer implements IDisposable {

    private ErlangBracketInserter fBracketInserter = null;

    public ErlangSourceViewer(final Composite parent, final IVerticalRuler ruler,
            final IOverviewRuler overviewRuler, final boolean showsAnnotationOverview,
            final int styles, final IBracketInserterValidator validator) {
        super(parent, ruler, overviewRuler, showsAnnotationOverview, styles);
        fBracketInserter = new ErlangBracketInserter(this);
        if (validator != null) {
            fBracketInserter.setValidator(validator);
        }
    }

    @Override
    public void configure(final SourceViewerConfiguration configuration) {
        super.configure(configuration);
        fBracketInserter.configure();
    }

    @Override
    public void dispose() {
        fBracketInserter.unconfigure();
    }

    public void setBracketInserterValidator(final IBracketInserterValidator validator) {
        fBracketInserter.setValidator(validator);
    }

    public static SourceViewer createErlangPreviewer(final Composite parent,
            final IColorManager colorManager0, final IPreferenceStore topStore,
            final List<TokenHighlight> colors0, final String content) {
        // TODO we should move this method, to a utility class (or maybe create
        // an ErlangPreviewSourceViewer class)
        final IColorManager colorManager = colorManager0 != null ? colorManager0
                : new ColorManager();

        List<TokenHighlight> colors;
        if (colors0 == null) {
            colors = new ArrayList<TokenHighlight>();
            for (final TokenHighlight th : TokenHighlight.values()) {
                colors.add(th);
            }
        } else {
            colors = colors0;
        }

        final IPreferenceStore generalTextStore = EditorsUI.getPreferenceStore();
        final IPreferenceStore store = topStore == null ? new ChainedPreferenceStore(
                new IPreferenceStore[] { generalTextStore })
                : new ChainedPreferenceStore(new IPreferenceStore[] { topStore,
                        generalTextStore });

        final SourceViewer viewer = new SourceViewer(parent, null, null, false,
                SWT.V_SCROLL | SWT.H_SCROLL | SWT.BORDER);
        final IDocument document = new Document(content);
        viewer.setDocument(document);

        final ErlangDocumentSetupParticipant setupParticipant = new ErlangDocumentSetupParticipant();
        setupParticipant.setup(document);

        final ErlangSourceViewerConfiguration configuration = new SyntaxColorPreviewEditorConfiguration(
                store, colorManager, colors);
        viewer.configure(configuration);

        final Font font = JFaceResources.getFont(PreferenceConstants.EDITOR_TEXT_FONT);
        viewer.getTextWidget().setFont(font);
        new ErlangSourceViewerUpdater(viewer, configuration, store);
        viewer.setEditable(false);

        final Cursor arrowCursor = viewer.getTextWidget().getDisplay()
                .getSystemCursor(SWT.CURSOR_ARROW);
        viewer.getTextWidget().setCursor(arrowCursor);

        return viewer;
    }
}
