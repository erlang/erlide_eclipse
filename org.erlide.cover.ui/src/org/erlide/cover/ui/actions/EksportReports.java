package org.erlide.cover.ui.actions;

import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import org.eclipse.jface.action.Action;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.erlide.cover.core.Logger;
import org.erlide.cover.ui.Activator;
import org.erlide.cover.ui.CoverageHelper;
import org.erlide.cover.ui.views.util.ReportGenerator;
import org.erlide.cover.views.model.ICoverageObject;
import org.erlide.cover.views.model.ObjectType;
import org.erlide.cover.views.model.StatsTreeModel;

/**
 * Exports HTML reports
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class EksportReports extends Action {

    private final Shell shell;

    private final Logger log = Activator.getDefault();

    public EksportReports(final Shell shell) {
        this.shell = shell;
    }

    @Override
    public void run() {

        final FileDialog fd = new FileDialog(shell, SWT.SAVE);
        fd.setText("Select directory to export your HTML reports");
        fd.setFilterExtensions(new String[] { "*.*" });
        final String path = fd.open();

        log.info(path);
        if (path == null) {
            return;
        }

        final File report = new File(path);
        report.mkdir();
        treeSave(StatsTreeModel.getInstance().getRoot(), path, path);

    }

    // saves reports as a tree
    private void treeSave(final ICoverageObject obj, final String path,
            final String lPath) {

        final String pathIn = obj.getHtmlPath();
        final String pathOut = new StringBuffer(path).append(File.separator)
                .append(obj.getLabel()).append(".html").toString();

        log.info("pathOut");
        obj.setRelativePath(new StringBuffer(lPath).append(File.separator)
                .append(obj.getLabel()).append(".html").toString());

        if (!obj.getType().equals(ObjectType.MODULE)) {
            final String linkPath = new StringBuilder(".")
                    .append(File.separator).append(obj.getLabel()).toString();
            final String dirPath = new StringBuilder(path)
                    .append(File.separator).append(obj.getLabel()).toString();
            final File dir = new File(dirPath);
            dir.mkdir();

            for (final ICoverageObject child : obj.getChildren()) {
                treeSave(child, dirPath, linkPath);
            }

            try {
                final String report = ReportGenerator.getInstance()
                        .getHTMLreport(obj, true);
                log.info(report);
                final FileWriter writer = new FileWriter(pathOut);
                writer.write(report);
                writer.close();
                obj.setHtmlPath(pathOut);

            } catch (final IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

        } else {
            final File input = new File(pathIn);
            final File output = new File(pathOut);

            try {
                final FileReader in = new FileReader(input);
                final FileWriter out = new FileWriter(output);

                int c;
                while ((c = in.read()) != -1) {
                    out.write(c);
                }

                in.close();
                out.close();
            } catch (final IOException e) {
                CoverageHelper.reportError("Could not export HTML reports");
                e.printStackTrace();
            }
        }
    }

}
