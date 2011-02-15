package org.erlide.cover.ui.annotations;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.log4j.Logger;
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
import org.erlide.cover.core.ICoverAnnotationMarker;
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
    private Logger log;

    private EditorTracker(final IWorkbench workbench) {
        this.workbench = workbench;
        log = Logger.getLogger(getClass());
        
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
                    annotateEditor(editor.getEditor(false));
                }
            }
        }
    }

    /**
     * Marks coverage for a specified file.
     * 
     * @param fileName
     */
    public void addAnnotationsToFile(String fileName) {

        IEditorPart currentEditor = workbench.getActiveWorkbenchWindow()
                .getActivePage().getActiveEditor();

        if (currentEditor.getTitle().equals(fileName)) {

            annotateEditor(currentEditor);
        }

    }

    public void removeAnnotationsFromFile(String fileName) {
        IEditorPart currentEditor = workbench.getActiveWorkbenchWindow()
                .getActivePage().getActiveEditor();

        if (currentEditor.getTitle().equals(fileName)) {

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
    public void addAnnotationsFragment(String fileName, int start, int end) {
        IEditorPart currentEditor = workbench.getActiveWorkbenchWindow()
                .getActivePage().getActiveEditor();

        if (currentEditor.getTitle().equals(fileName)
                && currentEditor instanceof ITextEditor) {

            ITextEditor editor = (ITextEditor) currentEditor;
            String modName = fileName.replace(".erl", "");
            ModuleStats module = ModuleSet.get(modName);

            if (module == null)
                return;

            List<LineResult> list = module.getLineResults();

            final Map<Position, Annotation> curAnn = buildAnnotationsMap(editor);

            for (final LineResult lr : list) {

                if (lr.getLineNum() < start
                        || (end != -1 && lr.getLineNum() > end))
                    continue;
                
                log.debug(lr.getLineNum());

                markLine(curAnn, editor, lr);

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
    public void removeAnnotationsFragment(String fileName, int start, int end) {

        IEditorPart currentEditor = workbench.getActiveWorkbenchWindow()
                .getActivePage().getActiveEditor();

        if (currentEditor.getTitle().equals(fileName)
                && currentEditor instanceof ITextEditor) {

            ITextEditor editor = (ITextEditor) currentEditor;
            IDocument doc = editor.getDocumentProvider().getDocument(
                    editor.getEditorInput());
            IAnnotationModel annMod = editor.getDocumentProvider()
                    .getAnnotationModel(editor.getEditorInput());

            final Map<Position, Annotation> curAnn = buildAnnotationsMap(editor);

            String modName = fileName.replace(".erl", "");
            ModuleStats module = ModuleSet.get(modName);

            if (module == null)
                return;

            List<LineResult> list = module.getLineResults();

            for (final LineResult lr : list) {

                if (lr.getLineNum() < start
                        || (end != -1 && lr.getLineNum() > end))
                    continue;
                
                log.debug(lr.getLineNum());

                try {

                    IRegion reg = doc.getLineInformation(lr.getLineNum() - 1);
                    int length = reg.getLength();
                    int offset = reg.getOffset();
                    Position pos = new Position(offset, length);
                    
                    Annotation annotation;
                    if( ((annotation = curAnn.get(pos)) != null) && 
                            (annotation.getType().equals(CoverageTypes.FULL_COVERAGE) ||
                                    annotation.getType().equals(CoverageTypes.NO_COVERAGE))) {
                        
                        annMod.removeAnnotation(annotation);
                    }
                    

                } catch (BadLocationException e) {
                    log.error(e);
                    e.printStackTrace();
                }
            }

        }
    }

    public void annotateEditor(final IWorkbenchPart part) {

        // TODO:add boundaries for line numbers

        if (!(part instanceof ITextEditor)) {
            return;
        }

        final ITextEditor editor = (ITextEditor) part;
        final String modName = editor.getTitle().replace(".erl", "");
        final ModuleStats module = ModuleSet.get(modName);

        if (module == null)
            return;

        final List<LineResult> list = module.getLineResults();

        final Map<Position, Annotation> curAnn = buildAnnotationsMap(editor);

        for (final LineResult lr : list) {

            if (lr.getLineNum() == 0)
                continue;

            markLine(curAnn, editor, lr);

        }

    }

    private Map<Position, Annotation> buildAnnotationsMap(
            final ITextEditor editor) {
        final Map<Position, Annotation> curAnn = new HashMap<Position, Annotation>();

        final IAnnotationModel annMod = editor.getDocumentProvider()
                .getAnnotationModel(editor.getEditorInput());

        final Iterator it = annMod.getAnnotationIterator();
        while (it.hasNext()) {
            final Annotation ann = (Annotation) it.next();
            curAnn.put(annMod.getPosition(ann), ann);
        }
        return curAnn;
    }

    private void markLine(final Map<Position, Annotation> curAnn,
            final ITextEditor editor, LineResult lr) {

        final IDocument doc = editor.getDocumentProvider().getDocument(
                editor.getEditorInput());

        final IAnnotationModel annMod = editor.getDocumentProvider()
                .getAnnotationModel(editor.getEditorInput());

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

            if (!curAnn.containsKey(pos)
                    || curAnn.containsKey(pos)
                    && curAnn.get(pos).getType()
                            .equals(CoverageTypes.NO_COVERAGE)
                    && annotation.getType().equals(
                            CoverageTypes.FULL_COVERAGE)) {
                annMod.addAnnotation(annotation, pos);
            }

        } catch (final BadLocationException e) {
            log.error(e);
            e.printStackTrace();
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
                    clearAnnotations(editor.getPart(false));
                }
            }
        }
    }

    public void clearAnnotations(final IWorkbenchPart part) {

        if (part instanceof ITextEditor) {
            final ITextEditor editor = (ITextEditor) part;

            final IAnnotationModel annMod = editor.getDocumentProvider()
                    .getAnnotationModel(editor.getEditorInput());

            final Iterator it = annMod.getAnnotationIterator();

            while (it.hasNext()) {
                final Annotation annotation = (Annotation) it.next();
                if (annotation.getType().equals(
                        CoverageTypes.FULL_COVERAGE)
                        || annotation.getType().equals(
                                CoverageTypes.NO_COVERAGE)) {
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
            annotateEditor(partref.getPart(false));
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