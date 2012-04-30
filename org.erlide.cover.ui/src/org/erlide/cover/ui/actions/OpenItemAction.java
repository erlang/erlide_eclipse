package org.erlide.cover.ui.actions;

import java.io.File;

import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.statushandlers.StatusManager;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.util.ErlangFunction;
import org.erlide.cover.core.Logger;
import org.erlide.cover.ui.Activator;
import org.erlide.cover.views.model.FunctionStats;
import org.erlide.cover.views.model.ModuleStats;
import org.erlide.cover.views.model.StatsTreeObject;
import org.erlide.ui.editors.erl.ErlangEditor;

/**
 * Action for opening items in the editor.
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class OpenItemAction extends Action {

    private final TreeViewer viewer;

    private final Logger log; // logger

    public OpenItemAction(final TreeViewer viewer) {
        log = Activator.getDefault();
        this.viewer = viewer;
    }

    @Override
    public void run() {

        log.info("Open item!");

        final ISelection selection = viewer.getSelection();

        if (!(selection instanceof ITreeSelection)) {
            final IStatus executionStatus = new Status(IStatus.ERROR,
                    Activator.PLUGIN_ID,
                    "Internall error occured: bad sellection type", null);
            StatusManager.getManager().handle(executionStatus,
                    StatusManager.SHOW);
            return;
        }

        final ITreeSelection treeSelection = (ITreeSelection) selection;

        final StatsTreeObject obj = (StatsTreeObject) treeSelection
                .getFirstElement();

        if (obj.getClass().equals(ModuleStats.class)) {

            openInEditor(obj.getLabel() + ".erl");

        } else if (obj.getClass().equals(FunctionStats.class)) {

            final FunctionStats fs = (FunctionStats) obj;

            final String moduleName = ((StatsTreeObject) fs.getParent())
                    .getLabel();
            final IEditorPart p = openInEditor(moduleName + ".erl");

            if (p == null || !(p instanceof ErlangEditor)) {
                return;
            }

            final ErlangEditor editor = (ErlangEditor) p;

            IErlModule module;
            try {
                module = ErlModelManager.getErlangModel()
                        .findModule(moduleName);

                final IErlFunction f = module.findFunction(new ErlangFunction(
                        fs.getLabel(), fs.getArity()));

                editor.setSelection(f);

            } catch (final ErlModelException e) {
                e.printStackTrace();
            }

        } else {
            // disabled
        }
    }

    private IEditorPart openInEditor(final String name) {

        // search

        IErlModule module;
        try {
            module = ErlModelManager.getErlangModel().findModule(name);
        } catch (final ErlModelException e1) {
            e1.printStackTrace();
            return null;
        }

        log.info(module.getFilePath());

        final File fileToOpen = new File(module.getFilePath());

        final IFileStore fileStore = EFS.getLocalFileSystem().getStore(
                fileToOpen.toURI());
        final IWorkbenchPage page = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();

        try {
            final IEditorPart p = IDE.openEditorOnFileStore(page, fileStore);

            log.info(p.getClass());

            return p;

        } catch (final PartInitException e) {
            e.printStackTrace();
            return null;
        }

    }

}
