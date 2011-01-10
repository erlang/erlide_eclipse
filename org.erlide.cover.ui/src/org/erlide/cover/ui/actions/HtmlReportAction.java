package org.erlide.cover.ui.actions;

import java.io.File;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.widgets.Shell;
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

    Shell shell;
    TreeViewer viewer;
    
    public HtmlReportAction(TreeViewer viewer) {
        this.shell = viewer.getControl().getShell();
        this.viewer = viewer;
    }
    
    public void run() {
        System.out.println("html report!");
        
        //viewer jest selection providerem
        
        ISelection selection = viewer.getSelection();
        
        System.out.println(selection.getClass().getName());
        if(!(selection instanceof ITreeSelection)) {
            //show error
        }
        
        ITreeSelection treeSelection = (ITreeSelection)selection;
        
        System.out.println(treeSelection.getFirstElement());
        System.out.println(treeSelection.getFirstElement().getClass().getName());
        System.out.println(treeSelection.getPaths());
        
        StatsTreeObject selObj = (StatsTreeObject)treeSelection.getFirstElement();
        
        BrowserDialog browser = new BrowserDialog(
                shell , SWT.DIALOG_TRIM | SWT.RESIZE);
        
        if (selObj instanceof FunctionStats) {
            ModuleStats module = (ModuleStats) selObj.getParent();
            browser.setFilePath(module.getHtmlPath());
        } else {
            browser.setFilePath(selObj.getHtmlPath());
        }
             
        browser.open();
    }
    
}
