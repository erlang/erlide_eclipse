package org.erlide.cover.ui.annotations;

import java.util.List;

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

    private final IWorkbench workbench;

    public EditorTracker(IWorkbench workbench) {
        this.workbench = workbench;

        IWorkbenchWindow[] windows = workbench.getWorkbenchWindows();

        for (IWorkbenchWindow w : windows) {
            w.getPartService().addPartListener(partListener);
        }

        workbench.addWindowListener(windowListener);
    }

    public void dispose() {
        workbench.removeWindowListener(windowListener);

        IWorkbenchWindow[] windows = workbench.getWorkbenchWindows();

        for (IWorkbenchWindow w : windows) {
            w.getPartService().removePartListener(partListener);
        }
    }

    public void addAnnotations() {
        IWorkbenchWindow[] windows = workbench.getWorkbenchWindows();

        System.out.println("Annotation maker!");

        for (IWorkbenchWindow w : windows) {
            for (IWorkbenchPage page : w.getPages()) {
                for (IEditorReference editor : page.getEditorReferences()) {
                    annotateEditor(editor);
                }
            }
        }
    }

    private void annotateEditor(IWorkbenchPartReference partref) {
        IWorkbenchPart part = partref.getPart(false);

        if (part instanceof ITextEditor) {
            ITextEditor editor = (ITextEditor) part;
            System.out.println(">> " + editor.getTitle());

            IDocument doc = editor.getDocumentProvider().getDocument(
                    editor.getEditorInput());

            StatsTreeModel model = StatsTreeModel.getInstance();
            System.out.println(model.getRoot().getChildren().getClass());
           

            String modName = editor.getTitle().replace(".erl", "");
            ModuleStats module = ModuleSet.get(modName);
            
            if(module == null)
                return;
            
            List<LineResult> list = module.getLineResults();
            for (LineResult lr : list) {
                if (lr.called()) {

                    try {
                        IRegion reg = doc.getLineInformation(lr.getLineNum() -1);
                        int length = reg.getLength();
                        int offset = reg.getOffset();
                        Position pos = new Position(offset, length);

                        Annotation annotation = new CoverageAnnotation(
                                CoverageAnnotation.FULL_COVERAGE);

                        IAnnotationModel annMod = editor.getDocumentProvider()
                                .getAnnotationModel(editor.getEditorInput());

                        annMod.addAnnotation(annotation, pos);

                    } catch (BadLocationException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }

                }
            }
            // }
            // }
        }
    }

    private IWindowListener windowListener = new IWindowListener() {

        public void windowOpened(IWorkbenchWindow window) {
            window.getPartService().addPartListener(partListener);
        }

        public void windowClosed(IWorkbenchWindow window) {
            window.getPartService().removePartListener(partListener);
        }

        public void windowActivated(IWorkbenchWindow window) {
        }

        public void windowDeactivated(IWorkbenchWindow window) {
        }

    };

    private IPartListener2 partListener = new IPartListener2() {

        public void partOpened(IWorkbenchPartReference partref) {
            annotateEditor(partref);
        }

        public void partActivated(IWorkbenchPartReference partref) {
        }

        public void partBroughtToTop(IWorkbenchPartReference partref) {
        }

        public void partVisible(IWorkbenchPartReference partref) {
        }

        public void partInputChanged(IWorkbenchPartReference partref) {
        }

        public void partClosed(IWorkbenchPartReference partref) {
        }

        public void partDeactivated(IWorkbenchPartReference partref) {
        }

        public void partHidden(IWorkbenchPartReference partref) {
        }
    };

}