package org.erlide.cover.ui.annotations;

import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.ui.IEditorPart;
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
import org.erlide.cover.core.Activator;
import org.erlide.cover.core.ICoverAnnotationMarker;
import org.erlide.cover.core.Logger;
import org.erlide.cover.views.model.LineResult;
import org.erlide.cover.views.model.ModuleSet;
import org.erlide.cover.views.model.ModuleStats;

/**
 * Attaches coverage annotation models.
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 */
public class EditorTracker implements ICoverAnnotationMarker {

    private static EditorTracker editorTracker;

    private final IWorkbench workbench;
    private final CoverageMap coverage;

    private final Logger log;

    private EditorTracker(final IWorkbench workbench) {
        this.workbench = workbench;
        log = Activator.getDefault();

        final IWorkbenchWindow[] windows = workbench.getWorkbenchWindows();

        for (final IWorkbenchWindow w : windows) {
            w.getPartService().addPartListener(partListener);
        }

        coverage = new CoverageMap();

        this.workbench.addWindowListener(windowListener);
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

    /**
     * Marks coverage of all tested modules (if they are opened)
     */
    @Override
    public void addAnnotations() {
        final IWorkbenchWindow[] windows = workbench.getWorkbenchWindows();

        createAnnotationMap();

        for (final IWorkbenchWindow w : windows) {
            for (final IWorkbenchPage page : w.getPages()) {
                for (final IEditorReference editor : page.getEditorReferences()) {
                    if (editor.isDirty()) {
                        continue;
                    }
                    annotateEditor(editor.getEditor(false));
                }
            }
        }
    }

    private void createAnnotationMap() {

        final Iterator<ModuleStats> it = ModuleSet.iterator();

        while (it.hasNext()) {
            final ModuleStats module = it.next();
            if (!module.couldBeMarked) {
                continue;
            }

            final List<LineResult> list = module.getLineResults();
            final String modName = module.getLabel() + ".erl";

            for (final LineResult lr : list) {

                coverage.addAnnotation(modName, lr, null);

            }
        }
    }

    /**
     * Marks coverage for a specified file.
     * 
     * @param fileName
     */
    public void addAnnotationsToFile(final String fileName) {

        final IEditorPart currentEditor = workbench.getActiveWorkbenchWindow()
                .getActivePage().getActiveEditor();

        if (currentEditor.getTitle().equals(fileName)
                && !currentEditor.isDirty()) {

            final ModuleStats module = ModuleSet.get(fileName.replace(".erl",
                    ""));

            if (module == null || !module.couldBeMarked) {
                return;
            }

            final List<LineResult> list = module.getLineResults();

            for (final LineResult lr : list) {
                coverage.addAnnotation(fileName, lr, null);
            }

            annotateEditor(currentEditor);
        }

    }

    public void removeAnnotationsFromFile(final String fileName) {
        final IEditorPart currentEditor = workbench.getActiveWorkbenchWindow()
                .getActivePage().getActiveEditor();

        if (currentEditor.getTitle().equals(fileName)) {

            coverage.removeAll(fileName);

            clearAnnotations(currentEditor);
        }
    }

    /**
     * Makrs coverage of fragment of a file, e.g. of a single function.
     * 
     * @param fileName
     * @param start
     * @param end
     */
    public void addAnnotationsFragment(final String fileName, final int start,
            final int end) {
        final IEditorPart currentEditor = workbench.getActiveWorkbenchWindow()
                .getActivePage().getActiveEditor();

        if (currentEditor.getTitle().equals(fileName)
                && currentEditor instanceof ITextEditor
                && !currentEditor.isDirty()) {

            final ModuleStats module = ModuleSet.get(fileName.replace(".erl",
                    ""));

            if (module == null || !module.couldBeMarked) {
                return;
            }

            final List<LineResult> list = module.getLineResults();

            final ITextEditor editor = (ITextEditor) currentEditor;

            log.info(fileName);

            for (final LineResult lr : list) {

                if (lr.getLineNum() < start || end != -1
                        && lr.getLineNum() > end) {
                    continue;
                }

                if (!coverage.containsAnnotation(fileName, lr)) {
                    coverage.addAnnotation(fileName, lr, null);
                }

                markLine(editor, lr);

            }

        }

    }

    /**
     * Removes coverage annotations from a fragment of file
     * 
     * @param fileName
     * @param start
     * @param end
     */
    public void removeAnnotationsFragment(final String fileName,
            final int start, final int end) {

        final IEditorPart currentEditor = workbench.getActiveWorkbenchWindow()
                .getActivePage().getActiveEditor();

        if (currentEditor.getTitle().equals(fileName)
                && currentEditor instanceof ITextEditor) {

            final ITextEditor editor = (ITextEditor) currentEditor;
            final IAnnotationModel annMod = editor.getDocumentProvider()
                    .getAnnotationModel(editor.getEditorInput());

            final Set<LineResult> list = coverage.getLineSet(editor.getTitle());

            for (final LineResult lr : list) {

                if (lr.getLineNum() < start || end != -1
                        && lr.getLineNum() > end) {
                    continue;
                }

                log.info(lr.getLineNum());
                if (coverage.containsAnnotation(editor.getTitle(), lr)) {
                    final Annotation ann = coverage.getAnnotation(
                            editor.getTitle(), lr);
                    annMod.removeAnnotation(ann);
                    coverage.removeAnnotation(editor.getTitle(), lr);
                }
            }
        }
    }

    public void annotateEditor(final IWorkbenchPart part) {

        if (!(part instanceof ITextEditor)) {
            return;
        }

        final ITextEditor editor = (ITextEditor) part;

        log.info(editor.getTitle());

        if (!coverage.containsFile(editor.getTitle())) {
            return;
        }

        log.info(coverage);

        final Set<LineResult> list = coverage.getLineSet(editor.getTitle());

        for (final LineResult lr : list) {

            if (lr.getLineNum() == 0) {
                continue;
            }

            markLine(editor, lr);

        }

    }

    private void markLine(final ITextEditor editor, final LineResult lr) {

        final IDocument doc = editor.getDocumentProvider().getDocument(
                editor.getEditorInput());

        final IAnnotationModel annMod = editor.getDocumentProvider()
                .getAnnotationModel(editor.getEditorInput());

        log.info("mark line " + lr.getLineNum());

        try {

            final IRegion reg = doc.getLineInformation(lr.getLineNum() - 1);
            final int length = reg.getLength();
            final int offset = reg.getOffset();
            final Position pos = new Position(offset, length);

            Annotation annotation;
            if (lr.called()) {
                annotation = CoverageAnnotationFactory
                        .create(CoverageTypes.FULL_COVERAGE);
            } else {
                annotation = CoverageAnnotationFactory
                        .create(CoverageTypes.NO_COVERAGE);
            }

            final Annotation lastAnn = coverage.getAnnotation(
                    editor.getTitle(), lr);

            log.info(lastAnn);

            if (lastAnn == null) {
                annMod.addAnnotation(annotation, pos);
                coverage.addAnnotation(editor.getTitle(), lr, annotation);
            } else if (annMod.getPosition(lastAnn) == null) {
                annMod.addAnnotation(lastAnn, pos);
            } else if (lastAnn.getType().equals(CoverageTypes.NO_COVERAGE)
                    && annotation.getType().equals(CoverageTypes.FULL_COVERAGE)) {

                annMod.removeAnnotation(lastAnn);
                annMod.addAnnotation(annotation, pos);
                coverage.addAnnotation(editor.getTitle(), lr, annotation);
            }

        } catch (final BadLocationException e) {
            log.error(e);
            e.printStackTrace();
        }

    }

    /**
     * clears coverage annotations from all files opened in the editor
     */
    @Override
    public void clearAllAnnotations() {
        final IWorkbenchWindow[] windows = workbench.getWorkbenchWindows();

        coverage.removeAll();

        for (final IWorkbenchWindow w : windows) {
            for (final IWorkbenchPage page : w.getPages()) {
                for (final IEditorReference editor : page.getEditorReferences()) {
                    clearAnnotations(editor.getPart(false));
                }
            }
        }
    }

    public void clearAnnotations(final IWorkbenchPart part) {

        if (part != null) {
            log.info(part.getTitle());
        }
        if (part instanceof ITextEditor) {
            final ITextEditor editor = (ITextEditor) part;

            final IAnnotationModel annMod = editor.getDocumentProvider()
                    .getAnnotationModel(editor.getEditorInput());

            @SuppressWarnings("rawtypes")
            final Iterator it = annMod.getAnnotationIterator();

            while (it.hasNext()) {
                final Annotation annotation = (Annotation) it.next();
                if (annotation.getType().equals(CoverageTypes.FULL_COVERAGE)
                        || annotation.getType().equals(
                                CoverageTypes.NO_COVERAGE)) {
                    annMod.removeAnnotation(annotation);
                }
            }
        }
    }

    private final IWindowListener windowListener = new IWindowListener() {

        @Override
        public void windowOpened(final IWorkbenchWindow window) {
            window.getPartService().addPartListener(partListener);
        }

        @Override
        public void windowClosed(final IWorkbenchWindow window) {
            window.getPartService().removePartListener(partListener);
        }

        @Override
        public void windowActivated(final IWorkbenchWindow window) {
        }

        @Override
        public void windowDeactivated(final IWorkbenchWindow window) {
        }

    };

    private final IPartListener2 partListener = new IPartListener2() {

        @Override
        public void partOpened(final IWorkbenchPartReference partref) {
            annotateEditor(partref.getPart(false));
        }

        @Override
        public void partActivated(final IWorkbenchPartReference partref) {
        }

        @Override
        public void partBroughtToTop(final IWorkbenchPartReference partref) {
        }

        @Override
        public void partVisible(final IWorkbenchPartReference partref) {
        }

        @Override
        public void partInputChanged(final IWorkbenchPartReference partref) {
        }

        @Override
        public void partClosed(final IWorkbenchPartReference partref) {
        }

        @Override
        public void partDeactivated(final IWorkbenchPartReference partref) {
        }

        @Override
        public void partHidden(final IWorkbenchPartReference partref) {
        }
    };

}
