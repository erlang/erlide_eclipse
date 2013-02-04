package org.erlide.ui.editors.erl;

import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.text.ITextInputListener;
import org.eclipse.jface.text.source.IOverviewRuler;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.jface.text.source.projection.ProjectionViewer;
import org.eclipse.swt.widgets.Composite;
import org.erlide.utils.ErlLogger;
import org.erlide.utils.IDisposable;

public class ErlangSourceViewer extends ProjectionViewer implements IDisposable {

    private final IDocumentListener documentListener;
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

        documentListener = new IDocumentListener() {
            @Override
            public void documentAboutToBeChanged(final DocumentEvent event) {
                updateTextBuffer(event);
            }

            @Override
            public void documentChanged(final DocumentEvent event) {
            }
        };
        addTextInputListener(new ITextInputListener() {
            @Override
            public void inputDocumentChanged(final IDocument oldInput,
                    final IDocument newInput) {
                if (newInput != null) {
                    newInput.addDocumentListener(documentListener);
                    final String text = newInput.get();

                    // TODO we need to be able to override this, for editors we
                    // can just send the file path initially
                    ErlLogger.debug("INITIAL");
                    updateTextBuffer(new DocumentEvent(newInput, 0, 0, text));
                }
            }

            @Override
            public void inputDocumentAboutToBeChanged(final IDocument oldInput,
                    final IDocument newInput) {
                if (oldInput != null) {
                    oldInput.removeDocumentListener(documentListener);
                }
            }
        });

    }

    private void updateTextBuffer(final DocumentEvent event) {
        // ErlLogger.debug("UPDATE BUFFER " + event);
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
