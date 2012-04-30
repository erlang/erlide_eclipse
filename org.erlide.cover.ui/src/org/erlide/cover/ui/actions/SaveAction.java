package org.erlide.cover.ui.actions;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;

import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.erlide.cover.core.Logger;
import org.erlide.cover.ui.Activator;
import org.erlide.cover.ui.CoverageHelper;
import org.erlide.cover.views.model.StatsTreeModel;

/**
 * An action for saving coverage results
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class SaveAction extends Action {

    public static final String DIR_NAME = "cover_stats";

    private final Shell shell;

    private final Logger log = Activator.getDefault();

    public SaveAction(final Shell shell) {
        this.shell = shell;
    }

    @Override
    public void run() {

        final StringBuilder statName = new StringBuilder();
        statName.append("cov_").append(
                StatsTreeModel.getInstance().getTimestamp());

        final IPath location = Activator.getDefault().getStateLocation()
                .append(DIR_NAME);
        final File dir = location.toFile();

        if (!dir.exists() && !dir.mkdir()) {
            CoverageHelper.reportError("Can not save results!");
            return;
        }

        // open input dialog

        final InputDialog nameDialog = new InputDialog(shell,
                "Saving coverage results",
                "Enter the name for saving coverage results",
                statName.toString(), new IInputValidator() {

                    @Override
                    public String isValid(final String newText) {

                        final String[] names = dir.list();

                        if (newText == null || newText.length() < 1) {
                            return "Name too short";
                        }

                        for (final String name : names) {
                            if (name.equals(newText)) {
                                return "Results file with the same name already exists";
                            }
                        }

                        return null;
                    }

                });

        nameDialog.open();

        String name = "";
        if (nameDialog.getReturnCode() == Window.OK) {
            name = nameDialog.getValue();
        } else {
            return;
        }

        //

        log.info(location);
        log.info(name);

        ObjectOutputStream objOutStream;
        try {
            objOutStream = new ObjectOutputStream(new FileOutputStream(location
                    .append(name).toString()));
            try {
                objOutStream.writeObject(StatsTreeModel.getInstance());
            } finally {
                objOutStream.close();
            }

        } catch (final FileNotFoundException e) {
            log.error("Error while openning stream");
            e.printStackTrace();
            CoverageHelper.reportError("Cannot save results");
        } catch (final IOException e) {
            log.error("Error while writing to a file");
            e.printStackTrace();
            CoverageHelper.reportError("Cannot save results");
        }

    }

}
