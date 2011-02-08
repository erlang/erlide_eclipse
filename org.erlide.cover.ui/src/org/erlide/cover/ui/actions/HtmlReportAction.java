package org.erlide.cover.ui.actions;

import org.apache.log4j.Logger;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.statushandlers.StatusManager;
import org.erlide.cover.ui.Activator;
import org.erlide.cover.ui.views.util.BrowserDialog;
import org.erlide.cover.views.model.FunctionStats;
import org.erlide.cover.views.model.ModuleStats;
import org.erlide.cover.views.model.StatsTreeObject;

/**
 * Action for showing html reports
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class HtmlReportAction extends Action {

    private Shell shell;
    private TreeViewer viewer;

    private Logger log;     // logger
    
    public HtmlReportAction(final TreeViewer viewer) {
        shell = viewer.getControl().getShell();
        this.viewer = viewer;
        log = Logger.getLogger(getClass());
    }

    @Override
    public void run() {
        log.debug("html report!");

        // viewer jest selection providerem

        final ISelection selection = viewer.getSelection();

        log.debug(selection.getClass().getName());
        if (!(selection instanceof ITreeSelection)) {
            final IStatus executionStatus = new Status(IStatus.ERROR,
                    Activator.PLUGIN_ID, "Internall error occured: bad sellection type", null);
            StatusManager.getManager().handle(executionStatus,
                    StatusManager.SHOW);
            return;
        }

        final ITreeSelection treeSelection = (ITreeSelection) selection;

        log.debug(treeSelection.getFirstElement());
        log.debug(treeSelection.getFirstElement().getClass().getName());
        log.debug(treeSelection.getPaths());

        final StatsTreeObject selObj = (StatsTreeObject) treeSelection
                .getFirstElement();

        final BrowserDialog browser = new BrowserDialog(shell, SWT.DIALOG_TRIM
                | SWT.RESIZE);

        if (selObj instanceof FunctionStats) {
            final ModuleStats module = (ModuleStats) selObj.getParent();
            browser.setFilePath(module.getHtmlPath());
        } else {
            browser.setFilePath(selObj.getHtmlPath());
        }

        browser.open();
    }

}
