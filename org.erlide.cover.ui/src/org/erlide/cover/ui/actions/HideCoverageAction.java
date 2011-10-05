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

    private final Logger log; // private

    public HideCoverageAction(final TreeViewer viewer) {
        super(viewer);

        log = Activator.getDefault();
    }

    @Override
    protected void perform(final StatsTreeObject selection) {

        if (selection instanceof ModuleStats) {
            final ModuleStats module = (ModuleStats) selection;
            final String name = module.getLabel() + ".erl";
            marker.removeAnnotationsFromFile(name);
        } else if (selection instanceof FunctionStats) {
            final FunctionStats fs = (FunctionStats) selection;
            final ModuleStats module = (ModuleStats) fs.getParent();
            final String name = module.getLabel() + ".erl";

            log.info(fs.getLineStart());
            log.info(fs.getLineEnd());

            marker.removeAnnotationsFragment(name, fs.getLineStart(),
                    fs.getLineEnd());

        } else {
            marker.clearAllAnnotations();
        }
    }

}
