package org.erlide.cover.ui.annotations;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWindowListener;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.cover.core.ICoverAnnotationMarker;
import org.erlide.cover.views.model.LineResult;
import org.erlide.cover.views.model.ModuleSet;
import org.erlide.cover.views.model.ModuleStats;
import org.erlide.cover.views.model.StatsTreeModel;

/**
 * Attaches coverage annotation models.
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 */
public class EditorTracker implements ICoverAnnotationMarker {

    private static EditorTracker editorTracker;

    private final IWorkbench workbench;

    private EditorTracker(final IWorkbench workbench) {
        this.workbench = workbench;

        final IWorkbenchWindow[] windows = workbench.getWorkbenchWindows();

        for (final IWorkbenchWindow w : windows) {
            w.getPartService().addPartListener(partListener);
        }

        workbench.addWindowListener(windowListener);
    }

    public static synchronized EditorTracker getInstance() {
        if (editorTracker == null) {
            editorTracker = new EditorTracker(PlatformUI.getWorkbench());
        }
        return editorTracker;
    }

    public void dispose() {
        workbench.removeWindowListener(windowListener);

        final IWorkbenchWindow[] windows = workbench.getWorkbenchWindows();

        for (final IWorkbenchWindow w : windows) {
            w.getPartService().removePartListener(partListener);
        }
    }

    public void addAnnotations() {
        final IWorkbenchWindow[] windows = workbench.getWorkbenchWindows();

        for (final IWorkbenchWindow w : windows) {
            for (final IWorkbenchPage page : w.getPages()) {
                for (final IEditorReference editor : page.getEditorReferences()) {
                    annotateEditor(editor);
                }
            }
        }
    }

    public void annotateEditor(final IWorkbenchPartReference partref) {
        final IWorkbenchPart part = partref.getPart(false);

        if (part instanceof ITextEditor) {
            final ITextEditor editor = (ITextEditor) part;

            final IDocument doc = editor.getDocumentProvider().getDocument(
                    editor.getEditorInput());

            final StatsTreeModel model = StatsTreeModel.getInstance();

            final String modName = editor.getTitle().replace(".erl", "");
            final ModuleStats module = ModuleSet.get(modName);

            if (module == null) {
                return;
            }

            final List<LineResult> list = module.getLineResults();

            final Map<Position, Annotation> curAnn = new HashMap<Position, Annotation>();

            final IAnnotationModel annMod = editor.getDocumentProvider()
                    .getAnnotationModel(editor.getEditorInput());

            final Iterator it = annMod.getAnnotationIterator();
            while (it.hasNext()) {
                final Annotation ann = (Annotation) it.next();
                curAnn.put(annMod.getPosition(ann), ann);
            }

            // clearAnnotations(partref);

            for (final LineResult lr : list) {

                if (lr.getLineNum() == 0) {
                    continue;
                }
                try {

                    final IRegion reg = doc
                            .getLineInformation(lr.getLineNum() - 1);
                    final int length = reg.getLength();
                    final int offset = reg.getOffset();
                    final Position pos = new Position(offset, length);

                    Annotation annotation;
                    if (lr.called()) {
                        annotation = CoverageAnnotation
                                .create(CoverageAnnotation.FULL_COVERAGE);
                    } else {
                        annotation = CoverageAnnotation
                                .create(CoverageAnnotation.NO_COVERAGE);
                    }

                    if (!curAnn.containsKey(pos)
                            || curAnn.containsKey(pos)
                            && curAnn.get(pos).getType()
                                    .equals(CoverageAnnotation.NO_COVERAGE)
                            && annotation.getType().equals(
                                    CoverageAnnotation.FULL_COVERAGE)) {
                        annMod.addAnnotation(annotation, pos);
                    }

                } catch (final BadLocationException e) {
                    e.printStackTrace();
                }

            }
        }
    }

    /**
     * clears coverage annotations from all files opened in the editor
     */
    public void clearAllAnnotations() {
        final IWorkbenchWindow[] windows = workbench.getWorkbenchWindows();

        for (final IWorkbenchWindow w : windows) {
            for (final IWorkbenchPage page : w.getPages()) {
                for (final IEditorReference editor : page.getEditorReferences()) {
                    clearAnnotations(editor);
                }
            }
        }
    }

    public void clearAnnotations(final IWorkbenchPartReference partref) {
        final IWorkbenchPart part = partref.getPart(false);

        if (part instanceof ITextEditor) {
            final ITextEditor editor = (ITextEditor) part;

            final IAnnotationModel annMod = editor.getDocumentProvider()
                    .getAnnotationModel(editor.getEditorInput());

            final Iterator it = annMod.getAnnotationIterator();

            while (it.hasNext()) {
                final Annotation annotation = (Annotation) it.next();
                if (annotation.getType().equals(
                        CoverageAnnotation.FULL_COVERAGE)
                        || annotation.getType().equals(
                                CoverageAnnotation.NO_COVERAGE)) {
                    annMod.removeAnnotation(annotation);
                }
            }
        }
    }

    private final IWindowListener windowListener = new IWindowListener() {

        public void windowOpened(final IWorkbenchWindow window) {
            window.getPartService().addPartListener(partListener);
        }

        public void windowClosed(final IWorkbenchWindow window) {
            window.getPartService().removePartListener(partListener);
        }

        public void windowActivated(final IWorkbenchWindow window) {
        }

        public void windowDeactivated(final IWorkbenchWindow window) {
        }

    };

    private final IPartListener2 partListener = new IPartListener2() {

        public void partOpened(final IWorkbenchPartReference partref) {
            annotateEditor(partref);
        }

        public void partActivated(final IWorkbenchPartReference partref) {
        }

        public void partBroughtToTop(final IWorkbenchPartReference partref) {
        }

        public void partVisible(final IWorkbenchPartReference partref) {
        }

        public void partInputChanged(final IWorkbenchPartReference partref) {
        }

        public void partClosed(final IWorkbenchPartReference partref) {
        }

        public void partDeactivated(final IWorkbenchPartReference partref) {
        }

        public void partHidden(final IWorkbenchPartReference partref) {
        }
    };

}