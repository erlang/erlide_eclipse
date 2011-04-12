package org.erlide.cover.ui.actions;

import org.eclipse.jface.viewers.TreeViewer;
import org.erlide.cover.core.Activator;
import org.erlide.cover.core.Logger;
import org.erlide.cover.views.model.FunctionStats;
import org.erlide.cover.views.model.ModuleStats;
import org.erlide.cover.views.model.StatsTreeObject;

/**
 * Hiding annotations for specific objects
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 *
 */
public class HideCoverageAction extends CoverageAction {

    private Logger log;         // private
    
    public HideCoverageAction(TreeViewer viewer) {
        super(viewer);
        
        log = Activator.getDefault();
    }
    
    @Override
    protected void perform(StatsTreeObject selection) {
        
        if (selection instanceof ModuleStats) {
            ModuleStats module = (ModuleStats)selection;
            String name = module.getLabel() + ".erl";
            marker.removeAnnotationsFromFile(name);
        } else if (selection instanceof FunctionStats) {
            FunctionStats fs = (FunctionStats) selection;
            ModuleStats module = (ModuleStats)(fs).getParent();
            String name = module.getLabel() + ".erl";
            
            log.info(fs.getLineStart());
            log.info(fs.getLineEnd());
            
            marker.removeAnnotationsFragment(name, 
                    fs.getLineStart(), fs.getLineEnd());
            
        } else {
            marker.clearAllAnnotations();
        }
    }

}
