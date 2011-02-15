package org.erlide.cover.ui.actions;

import org.apache.log4j.Logger;
import org.eclipse.jface.viewers.TreeViewer;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlFunction;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.util.ErlangFunction;
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

	private Logger log;
	
    public ShowCoverageAction(TreeViewer viewer) {
        super(viewer);
        log = Logger.getLogger(this.getClass());
    }

    @Override
    protected void perform(StatsTreeObject selection) {
        
        if (selection instanceof ModuleStats) {
            ModuleStats module = (ModuleStats)selection;
            String name = module.getLabel() + ".erl";
            marker.addAnnotationsToFile(name);
        } else if (selection instanceof FunctionStats) {
        	FunctionStats fs = (FunctionStats)selection;
            ModuleStats module = (ModuleStats)fs.getParent();
            String name = module.getLabel() + ".erl";
            
            log.debug(fs.getLineStart());
            log.debug(fs.getLineEnd());
            
            marker.addAnnotationsFragment(name,
                    fs.getLineStart(),
                    fs.getLineEnd());
            
        } else {
            marker.addAnnotations();
        }
        
    }

}
