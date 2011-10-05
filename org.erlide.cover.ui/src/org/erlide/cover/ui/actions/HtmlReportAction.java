package org.erlide.cover.ui.actions;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.statushandlers.StatusManager;
import org.erlide.cover.constants.CoverConstants;
import org.erlide.cover.core.Logger;
import org.erlide.cover.ui.Activator;
import org.erlide.cover.ui.CoverageHelper;
import org.erlide.cover.ui.views.util.BrowserDialog;
import org.erlide.cover.ui.views.util.ReportGenerator;
import org.erlide.cover.views.model.FunctionStats;
import org.erlide.cover.views.model.ICoverageObject;
import org.erlide.cover.views.model.ModuleStats;
import org.erlide.cover.views.model.ObjectType;
import org.erlide.cover.views.model.StatsTreeModel;
import org.erlide.cover.views.model.StatsTreeObject;

/**
 * Action for showing html reports
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class HtmlReportAction extends Action {

    private final Shell shell;
    private final TreeViewer viewer;

    private final Logger log; // logger

    public HtmlReportAction(final TreeViewer viewer) {
        shell = viewer.getControl().getShell();
        this.viewer = viewer;
        log = Activator.getDefault();
    }

    @Override
    public void run() {
        log.info("html report!");

        // viewer jest selection providerem

        if (StatsTreeModel.getInstance().getRoot().getHtmlPath() == null) {
            final IPath location = Activator.getDefault().getStateLocation()
                    .append(CoverConstants.REPORT_DIR);
            final File dir = location.toFile();

            if (!dir.exists() && !dir.mkdir()) {
                CoverageHelper.reportError("Can not save results!");
                return;
            }
            generateReportTree(StatsTreeModel.getInstance().getRoot(),
                    dir.getAbsolutePath());
        }

        final ISelection selection = viewer.getSelection();

        log.info(selection.getClass().getName());
        if (!(selection instanceof ITreeSelection)) {
            final IStatus executionStatus = new Status(IStatus.ERROR,
                    Activator.PLUGIN_ID,
                    "Internall error occured: bad sellection type", null);
            StatusManager.getManager().handle(executionStatus,
                    StatusManager.SHOW);
            return;
        }

        final ITreeSelection treeSelection = (ITreeSelection) selection;

        log.info(treeSelection.getFirstElement());
        log.info(treeSelection.getFirstElement().getClass().getName());
        log.info(treeSelection.getPaths());

        final StatsTreeObject selObj = (StatsTreeObject) treeSelection
                .getFirstElement();

        final BrowserDialog browser = new BrowserDialog(shell);

        if (selObj instanceof FunctionStats) {
            final ModuleStats module = (ModuleStats) selObj.getParent();
            browser.setObject(module);
        } else {
            browser.setObject(selObj);
        }

        browser.open();
    }

    /**
     * Generates html reports
     * 
     * @param obj
     * @param path
     */
    private void generateReportTree(final ICoverageObject obj, final String path) {
        if (obj.getType().equals(ObjectType.MODULE)) {
            return;
        }
        final String reportPath = new StringBuilder(path)
                .append(File.separator).append(obj.getLabel()).append(".html")
                .toString();
        log.info(reportPath);

        final String dirPath = new StringBuilder(path).append(File.separator)
                .append(obj.getLabel()).toString();
        final File dir = new File(dirPath);
        dir.mkdir();

        for (final ICoverageObject child : obj.getChildren()) {
            generateReportTree(child, dirPath);
        }

        try {
            final String report = ReportGenerator.getInstance().getHTMLreport(
                    obj, false);
            log.info(report);
            final FileWriter writer = new FileWriter(reportPath);
            writer.write(report);
            writer.close();
            obj.setHtmlPath(reportPath);

        } catch (final IOException e) {
            e.printStackTrace();
        }

        for (final ICoverageObject child : obj.getChildren()) {
            generateReportTree(child, dirPath);
        }
    }
}
