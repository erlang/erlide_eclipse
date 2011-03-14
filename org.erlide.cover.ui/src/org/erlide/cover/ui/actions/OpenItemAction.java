package org.erlide.cover.ui.actions;

import java.io.File;

import org.apache.log4j.Logger;
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
import org.erlide.core.ErlangCore;
import org.erlide.core.model.erlang.ErlModelException;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.util.ErlangFunction;
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

        	FunctionStats fs = (FunctionStats)obj;
        	
        	String moduleName = ((StatsTreeObject)fs.getParent()).getLabel();
            IEditorPart p = openInEditor(moduleName + ".erl");
            
            if(p == null || !(p instanceof ErlangEditor))
            	return;
            
            ErlangEditor editor = (ErlangEditor)p;
            
            IErlModule module;
            try {
                module = ErlangCore.getModel().findModule(moduleName);
                
                IErlFunction f = module.findFunction(
                        new ErlangFunction(fs.getLabel(), fs.getArity()));
                
                editor.setSelection(f);
                
            } catch (ErlModelException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            
        } else {
            // TODO: should be disabled
        }
    }

    private IEditorPart openInEditor(String name) {

        // search
    	
    	IErlModule module;
        try {
            module = ErlangCore.getModel().findModule(name);
        } catch (ErlModelException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
            return null;
        }
    	
    	log.debug(module.getFilePath());
    	
    	
    	File fileToOpen = new File(module.getFilePath());
    	
        IFileStore fileStore = EFS.getLocalFileSystem().getStore(
                fileToOpen.toURI());
        IWorkbenchPage page = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();

        try {
            IEditorPart p = IDE.openEditorOnFileStore(page, fileStore);
            
            log.debug(p.getClass());
            
            return p;
            
        } catch (PartInitException e) {
            e.printStackTrace();
            return null;
        }

    }

}
