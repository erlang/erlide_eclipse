package org.erlide.ui.editors.erl;

import java.util.Iterator;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.texteditor.AnnotationPreference;
import org.eclipse.ui.texteditor.AnnotationPreferenceLookup;
import org.erlide.ui.editors.erl.hover.ErlangAnnotationIterator;
import org.erlide.ui.editors.erl.hover.IErlangAnnotation;

public class AnnotationSupport {
    private final ErlangEditor editor;
    private final AnnotationPreferenceLookup annotationPreferenceLookup;

    public AnnotationSupport(final ErlangEditor editor,
            final AnnotationPreferenceLookup annotationPreferenceLookup) {
        this.editor = editor;
        this.annotationPreferenceLookup = annotationPreferenceLookup;
    }

    /**
     * Returns whether the given annotation is configured as a target for the
     * "Go to Next/Previous Annotation" actions
     *
     * @param annotation
     *            the annotation
     * @return <code>true</code> if this is a target, <code>false</code>
     *         otherwise
     * @since 3.0
     */
    public boolean isNavigationTarget(final Annotation annotation) {
        final IPreferenceStore preferences = EditorsUI.getPreferenceStore();
        final AnnotationPreference preference = annotationPreferenceLookup
                .getAnnotationPreference(annotation);
        final String key = preference == null ? null : preference
                .getIsGoToNextNavigationTargetKey();
        return key != null && preferences.getBoolean(key);
    }

    public Annotation gotoAnnotation(final boolean forward) {
        final ITextSelection selection = (ITextSelection) editor.getSelectionProvider()
                .getSelection();
        final Position position = new Position(0, 0);
        final Annotation annotation = getNextAnnotation(selection.getOffset(),
                selection.getLength(), forward, position);
        editor.setStatusLineErrorMessage(null);
        editor.setStatusLineMessage(null);
        if (annotation != null) {
            updateAnnotationViews(annotation);
            editor.selectAndReveal(position.getOffset(), position.getLength());
            editor.setStatusLineMessage(annotation.getText());
        }
        return annotation;
    }

    /**
     * Returns the annotation closest to the given range respecting the given
     * direction. If an annotation is found, the annotation's current position
     * is copied into the provided position.
     */
    private Annotation getNextAnnotation(final int offset, final int length,
            final boolean forward, final Position annotationPosition) {

        Annotation nextAnnotation = null;
        Position nextAnnotationPosition = null;
        Annotation containingAnnotation = null;
        Position containingAnnotationPosition = null;
        boolean currentAnnotation = false;

        final IDocument document = editor.getDocumentProvider().getDocument(
                editor.getEditorInput());
        final int endOfDocument = document.getLength();
        int distance = Integer.MAX_VALUE;

        final IAnnotationModel model = editor.getDocumentProvider().getAnnotationModel(
                editor.getEditorInput());
        final Iterator<Annotation> e = new ErlangAnnotationIterator(model, true, true);
        while (e.hasNext()) {
            final Annotation a = e.next();
            if (a instanceof IErlangAnnotation && ((IErlangAnnotation) a).hasOverlay()
                    || !isNavigationTarget(a)) {
                continue;
            }

            final Position p = model.getPosition(a);
            if (p == null) {
                continue;
            }

            if (forward && p.offset == offset || !forward
                    && p.offset + p.getLength() == offset + length) {
                // || p.includes(offset))
                if (containingAnnotation == null
                        || containingAnnotationPosition != null
                        && (forward && p.length >= containingAnnotationPosition.length || !forward
                                && p.length < containingAnnotationPosition.length)) {
                    containingAnnotation = a;
                    containingAnnotationPosition = p;
                    currentAnnotation = p.length == length;
                }
            } else {
                int currentDistance = 0;

                if (forward) {
                    currentDistance = p.getOffset() - offset;
                    if (currentDistance < 0) {
                        currentDistance = endOfDocument + currentDistance;
                    }

                    if (currentDistance < distance || currentDistance == distance
                            && nextAnnotationPosition != null
                            && p.length < nextAnnotationPosition.length) {
                        distance = currentDistance;
                        nextAnnotation = a;
                        nextAnnotationPosition = p;
                    }
                } else {
                    currentDistance = offset + length - (p.getOffset() + p.length);
                    if (currentDistance < 0) {
                        currentDistance = endOfDocument + currentDistance;
                    }

                    if (currentDistance < distance || currentDistance == distance
                            && nextAnnotationPosition != null
                            && p.length < nextAnnotationPosition.length) {
                        distance = currentDistance;
                        nextAnnotation = a;
                        nextAnnotationPosition = p;
                    }
                }
            }
        }
        if (containingAnnotationPosition != null
                && (!currentAnnotation || nextAnnotation == null)) {
            annotationPosition.setOffset(containingAnnotationPosition.getOffset());
            annotationPosition.setLength(containingAnnotationPosition.getLength());
            return containingAnnotation;
        }
        if (nextAnnotationPosition != null) {
            annotationPosition.setOffset(nextAnnotationPosition.getOffset());
            annotationPosition.setLength(nextAnnotationPosition.getLength());
        }

        return nextAnnotation;
    }

    private void updateAnnotationViews(final Annotation annotation) {
    }

}
