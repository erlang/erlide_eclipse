package org.erlide.cover.ui.actions;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;

import org.apache.log4j.Logger;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.ui.statushandlers.StatusManager;
import org.erlide.cover.ui.Activator;
import org.erlide.cover.views.model.StatsTreeModel;

/**
 * An action for saving coverage results
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class SaveAction extends Action {

    public static final String DIR_NAME = "cover_stats";

    private Logger log = Logger.getLogger(getClass()); // logger
    

    @Override
    public void run() {

        StringBuilder statName = new StringBuilder();
        statName.append("cov_").append(
                StatsTreeModel.getInstance().getTimestamp());

        IPath location = Activator.getDefault().getStateLocation()
                .append(DIR_NAME);
        File dir = location.toFile();

        log.debug(location);

        if (!dir.exists() && !dir.mkdir()) {
            final IStatus executionStatus = new Status(IStatus.ERROR,
                    Activator.PLUGIN_ID, "Can not save results!", null);
            StatusManager.getManager().handle(executionStatus,
                    StatusManager.SHOW);
            return;
        }

        ObjectOutputStream objOutStream;
        try {
            objOutStream = new ObjectOutputStream(new FileOutputStream(location
                    .append(statName.toString()).toString()));

            objOutStream.writeObject(StatsTreeModel.getInstance());

        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

}
