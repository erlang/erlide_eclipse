package org.erlide.ui.editors.erl;

import org.eclipse.jface.text.source.IOverviewRuler;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.jface.text.source.projection.ProjectionViewer;
import org.eclipse.swt.widgets.Composite;
import org.erlide.utils.IDisposable;

public class ErlangSourceViewer extends ProjectionViewer implements IDisposable {

    private ErlangViewerBracketInserter fBracketInserter = null;

    public ErlangSourceViewer(final Composite parent,
            final IVerticalRuler ruler, final IOverviewRuler overviewRuler,
            final boolean showsAnnotationOverview, final int styles,
            final IBracketInserterValidator validator) {
        super(parent, ruler, overviewRuler, showsAnnotationOverview, styles);
        fBracketInserter = new ErlangViewerBracketInserter(this);
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

    public void setBracketInserterValidator(
            final IBracketInserterValidator validator) {
        fBracketInserter.setValidator(validator);
    }
}
