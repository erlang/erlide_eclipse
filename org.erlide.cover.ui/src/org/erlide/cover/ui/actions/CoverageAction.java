package org.erlide.cover.ui.actions;

import org.apache.log4j.Logger;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.ui.statushandlers.StatusManager;
import org.erlide.cover.ui.Activator;
import org.erlide.cover.ui.annotations.EditorTracker;
import org.erlide.cover.views.model.StatsTreeObject;

/**
 * Abstract class for the action of showing and hiding annotations
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 *
 */
public abstract class CoverageAction extends Action {
    
    protected EditorTracker marker;
    private TreeViewer viewer;
    
    private Logger log;     //logger

    public CoverageAction(TreeViewer viewer) {
        log = Logger.getLogger(getClass());
        this.viewer = viewer;
        marker = EditorTracker.getInstance();
    }
    
    @Override
    public void run() {
        
        log.debug("show coverage!");
        
        ISelection selection = viewer.getSelection();
        
        if (!(selection instanceof ITreeSelection)) {
            final IStatus executionStatus = new Status(IStatus.ERROR,
                    Activator.PLUGIN_ID, "Internall error occured: bad sellection type", null);
            StatusManager.getManager().handle(executionStatus,
                    StatusManager.SHOW);
            return;
        }

        ITreeSelection treeSelection = (ITreeSelection) selection;
        
        log.debug(treeSelection.getFirstElement().getClass());
        
        perform((StatsTreeObject)treeSelection.getFirstElement());
    }
    
    protected abstract void perform(StatsTreeObject selection);
    
}
