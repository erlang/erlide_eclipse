package org.erlide.test_support.ui.trace;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.IAnnotationModelExtension;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.texteditor.IDocumentProvider;

public class TraceEditor extends TextEditor {

    private Annotation callMatchingLine = null;
    private ISelectionChangedListener fEditorSelectionChangedListener;

    public TraceEditor() {
    }

    @Override
    protected void initializeEditor() {
        super.initializeEditor();

        final TraceEditorConfiguration cfg = new TraceEditorConfiguration(
                getPreferenceStore(), this);
        setSourceViewerConfiguration(cfg);
    }

    @Override
    public void createPartControl(final Composite parent) {
        super.createPartControl(parent);
        getSourceViewer().getTextWidget().addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(final MouseEvent e) {
                super.mouseDown(e);
                markCall();
            }
        });
    }

    @Override
    public boolean isEditable() {
        return false;
    }

    @Override
    public boolean isEditorInputModifiable() {
        return false;
    }

    @Override
    public boolean isEditorInputReadOnly() {
        return true;
    }

    @Override
    public boolean isDirty() {
        return false;
    }

    public void markCall() {
        final IDocumentProvider documentProvider = getDocumentProvider();
        final IAnnotationModel annotationModel = documentProvider
                .getAnnotationModel(getEditorInput());

        final ITextSelection pos = (ITextSelection) getSelectionProvider()
                .getSelection();
        final MarkCallLocation location = new MarkCallLocation(
                getSourceViewer().getDocument(), pos.getStartLine());

        final Position position = new Position(location.getOffset(),
                location.getLength());
        final String description = location.getDescription();

        final Annotation oldAnnotation = callMatchingLine;
        if (position.getOffset() != 0) {
            callMatchingLine = new Annotation(
                    "org.erlide.test_support.trace.call", false, description);
        } else {
            callMatchingLine = null;
        }

        if (annotationModel instanceof IAnnotationModelExtension) {
            final Map<Annotation, Position> annotationMap = new HashMap<Annotation, Position>();
            if (callMatchingLine != null) {
                annotationMap.put(callMatchingLine, position);
            }
            ((IAnnotationModelExtension) annotationModel).replaceAnnotations(
                    new Annotation[] { oldAnnotation }, annotationMap);
        } else {
            annotationModel.addAnnotation(callMatchingLine, position);
        }
    }

    private class MarkCallLocation {
        private String description;
        private int length;
        private int offset;

        public MarkCallLocation(final IDocument doc, final int line) {
            try {
                final IRegion info = doc.getLineInformation(line);
                final String lineStr = doc.get(info.getOffset(),
                        info.getLength());

                try {
                    final String cmd = lineStr.substring(26, 32);
                    if (cmd.equals("return")) {
                        String ref = lineStr.substring(34,
                                lineStr.indexOf(" ->"));
                        ref = ref.substring(0, ref.length() - 2);
                        // find call before it
                        for (int ln = line - 1; ln > 0; ln--) {
                            final IRegion li = doc.getLineInformation(ln);
                            final String ls = doc.get(li.getOffset(),
                                    li.getLength());
                            if (ls.contains(ref)) {
                                offset = li.getOffset();
                                length = li.getLength();
                                description = ref;
                                break;
                            }
                        }
                    } else if (cmd.equals("  call")) {
                        final String ref = lineStr.substring(34,
                                lineStr.indexOf('('));
                        // find return after it
                        for (int ln = line + 1; ln < doc.getNumberOfLines(); ln++) {
                            final IRegion li = doc.getLineInformation(ln);
                            final String ls = doc.get(li.getOffset(),
                                    li.getLength());
                            if (ls.contains(ref)) {
                                offset = li.getOffset();
                                length = li.getLength();
                                description = ref;
                                break;
                            }
                        }
                    } else {
                        // do nothing
                    }
                } catch (final Exception e) {
                    // ignore
                }
            } catch (final BadLocationException e) {
                e.printStackTrace();
            }
        }

        public int getOffset() {
            return offset;
        }

        public int getLength() {
            return length;
        }

        public String getDescription() {
            return description;
        }

    }

}
