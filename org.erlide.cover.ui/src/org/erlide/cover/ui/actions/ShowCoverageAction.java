package org.erlide.cover.ui.actions;

import java.io.File;
import java.util.Collection;

import org.eclipse.jface.viewers.TreeViewer;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.cover.core.Activator;
import org.erlide.cover.core.Logger;
import org.erlide.cover.core.MD5Checksum;
import org.erlide.cover.views.model.FunctionStats;
import org.erlide.cover.views.model.ICoverageObject;
import org.erlide.cover.views.model.ModuleStats;
import org.erlide.cover.views.model.StatsTreeModel;
import org.erlide.cover.views.model.StatsTreeObject;

/**
 * Showing annotations from context menu for specified objects
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class ShowCoverageAction extends CoverageAction {

    private final Logger log;

    public ShowCoverageAction(final TreeViewer viewer) {
        super(viewer);
        log = Activator.getDefault();
    }

    @Override
    protected void perform(final StatsTreeObject selection) {

        if (selection instanceof ModuleStats) {
            final ModuleStats module = (ModuleStats) selection;
            final String name = module.getLabel() + ".erl";
            if (ifMarkAnnotations(module)) {
                module.couldBeMarked = true;
                marker.addAnnotationsToFile(name);
            }
        } else if (selection instanceof FunctionStats) {
            final FunctionStats fs = (FunctionStats) selection;
            final ModuleStats module = (ModuleStats) fs.getParent();
            final String name = module.getLabel() + ".erl";

            if (ifMarkAnnotations(module)) {
                log.info(fs.getLineStart());
                log.info(fs.getLineEnd());
                module.couldBeMarked = true;
                marker.addAnnotationsFragment(name, fs.getLineStart(),
                        fs.getLineEnd());
            }

        } else if (selection.equals(StatsTreeModel.getInstance().getRoot())) {
            // TODO: check annotation tree, only if root mark all annotations
            final Collection<ICoverageObject> col = selection.getModules();
            for (final ICoverageObject module : col) {
                if (ifMarkAnnotations((ModuleStats) module)) {
                    ((ModuleStats) module).couldBeMarked = true;
                } else {
                    ((ModuleStats) module).couldBeMarked = false;
                }
            }
            marker.addAnnotations();
        } else {
            final Collection<ICoverageObject> col = selection.getModules();
            for (final ICoverageObject module : col) {
                if (ifMarkAnnotations((ModuleStats) module)) {
                    final String name = module.getLabel() + ".erl";
                    ((ModuleStats) module).couldBeMarked = true;
                    marker.addAnnotationsToFile(name);
                }
            }
        }

    }

    // calculate md5
    private boolean ifMarkAnnotations(final ModuleStats module) {
        try {
            final File file = new File(ErlModelManager.getErlangModel()
                    .findModule(module.getLabel()).getFilePath());

            if (module.getMd5().equals(MD5Checksum.getMD5(file))) {
                return true;
            }
        } catch (final Exception e) {
            // TODO
            e.printStackTrace();
        }
        return false;
    }

}
