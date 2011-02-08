package org.erlide.cover.ui.actions;

import org.eclipse.jface.viewers.TreeViewer;
import org.erlide.cover.views.model.FunctionStats;
import org.erlide.cover.views.model.ModuleStats;
import org.erlide.cover.views.model.StatsTreeObject;

/**
 * Showing annotations from context menu for specified objects
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 *
 */
public class ShowCoverageAction extends CoverageAction {

    public ShowCoverageAction(TreeViewer viewer) {
        super(viewer);
    }

    @Override
    protected void perform(StatsTreeObject selection) {
        
        if (selection instanceof ModuleStats) {
            ModuleStats module = (ModuleStats)selection;
            String name = module.getLabel() + ".erl";
            marker.addAnnotationsToFile(name);
        } else if (selection instanceof FunctionStats) {
            ModuleStats module = (ModuleStats)((FunctionStats)selection).getParent();
            String name = module.getLabel() + ".erl";
            marker.addAnnotationsToFile(name);
        } else {
            marker.addAnnotations();
        }
        
    }

}
