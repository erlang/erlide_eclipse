package org.erlide.cover.ui.actions;

import java.io.File;

import org.apache.log4j.Logger;
import org.eclipse.jface.viewers.TreeViewer;
import org.erlide.core.ErlangCore;
import org.erlide.cover.core.MD5Checksum;
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
            ModuleStats module = (ModuleStats) selection;
            String name = module.getLabel() + ".erl";
            if (ifMarkAnnotations(module))
                marker.addAnnotationsToFile(name);
        } else if (selection instanceof FunctionStats) {
            FunctionStats fs = (FunctionStats) selection;
            ModuleStats module = (ModuleStats) fs.getParent();
            String name = module.getLabel() + ".erl";

            if (ifMarkAnnotations(module)) {
                log.debug(fs.getLineStart());
                log.debug(fs.getLineEnd());

                marker.addAnnotationsFragment(name, fs.getLineStart(),
                        fs.getLineEnd());
            }

        } else {
            //TODO: check annotation tree, only if root mark all annotations
            marker.addAnnotations();
        }

    }

    private boolean ifMarkAnnotations(ModuleStats module) {
        // calculate md5

        try {
            File file = new File(ErlangCore.getModel()
                    .findModule(module.getLabel()).getFilePath());

            if (module.getMd5().equals(MD5Checksum.getMD5(file)))
                return true;
        } catch (Exception e) {
            // TODO
            e.printStackTrace();
        }
        return false;
        //
    }

}
