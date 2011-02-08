package org.erlide.cover.ui.actions;

import java.io.File;
import java.util.Collection;

import org.apache.commons.io.FileUtils;
import org.apache.log4j.Logger;
import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.statushandlers.StatusManager;
import org.erlide.cover.ui.Activator;
import org.erlide.cover.views.model.FunctionStats;
import org.erlide.cover.views.model.ModuleStats;
import org.erlide.cover.views.model.StatsTreeObject;

/**
 * Action for opening items in the editor.
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class OpenItemAction extends Action {

    private TreeViewer viewer;

    private Logger log; // logger

    public OpenItemAction(TreeViewer viewer) {
        log = Logger.getLogger(getClass());
        this.viewer = viewer;
    }

    @Override
    public void run() {

        log.debug("Open item!");

        ISelection selection = viewer.getSelection();

        if (!(selection instanceof ITreeSelection)) {
            final IStatus executionStatus = new Status(IStatus.ERROR,
                    Activator.PLUGIN_ID,
                    "Internall error occured: bad sellection type", null);
            StatusManager.getManager().handle(executionStatus,
                    StatusManager.SHOW);
            return;
        }

        ITreeSelection treeSelection = (ITreeSelection) selection;

        StatsTreeObject obj = (StatsTreeObject) treeSelection.getFirstElement();

        if (obj.getClass().equals(ModuleStats.class)) {
            openInEditor(obj.getLabel() + ".erl");
        } else if (obj.getClass().equals(FunctionStats.class)) {
            // TODO: should be opened on the specified function
            openInEditor(((StatsTreeObject) obj.getParent()).getLabel()
                    + ".erl");
        } else {
            // TODO: should be disabled
        }
    }

    private void openInEditor(String name) {

        // search
        IPath root = ResourcesPlugin.getWorkspace().getRoot().getRawLocation();

        Collection<File> files = FileUtils.listFiles(
                root.toFile(), new String[]{"erl"}, true);
        
        File fileToOpen = null;
        for(File f : files) {
            if(f.getName().equals(name))
                fileToOpen = f;
        }
        
        if(fileToOpen == null)
            return;
        
        //open
        IFileStore fileStore = EFS.getLocalFileSystem().getStore(
                fileToOpen.toURI());
        IWorkbenchPage page = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();

        try {
            IDE.openEditorOnFileStore(page, fileStore);
        } catch (PartInitException e) {
            e.printStackTrace();
        }

    }

}
