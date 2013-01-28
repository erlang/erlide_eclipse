package org.erlide.ui.editors.erl;

import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.text.ITextInputListener;
import org.eclipse.jface.text.source.IOverviewRuler;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.jface.text.source.projection.ProjectionViewer;
import org.eclipse.swt.widgets.Composite;
import org.erlide.utils.ErlLogger;

public class ErlangSourceViewer extends ProjectionViewer {

    private final IDocumentListener documentListener;

    public ErlangSourceViewer(final Composite parent,
            final IVerticalRuler ruler, final IOverviewRuler overviewRuler,
            final boolean showsAnnotationOverview, final int styles) {
        super(parent, ruler, overviewRuler, showsAnnotationOverview, styles);

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
        ErlLogger.debug("UPDATE BUFFER " + event);
    }

}
