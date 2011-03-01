package org.erlide.cover.ui.actions;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.log4j.Logger;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.eclipse.ui.statushandlers.StatusManager;
import org.erlide.cover.ui.Activator;

/**
 * An action for restoring coverage results which were previously saved
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions com>
 * 
 */
public class RestoreAction extends Action {

    private Shell shell;

    private Logger log; // logger

    public RestoreAction(Shell shell) {
        this.shell = shell;
        log = Logger.getLogger(getClass());
    }

    @Override
    public void run() {

        IPath location = Activator.getDefault().getStateLocation()
                .append(SaveAction.DIR_NAME);
        final File dir = location.toFile();
        
        if (!dir.exists() && !dir.mkdir()) {
            reportError("Can not save results!");
            return;
        }

     // open dialog
        ElementListSelectionDialog resDialog = new ElementListSelectionDialog(
                shell, labelProvider);
        
        resDialog.setElements(dir.listFiles());
        resDialog.setTitle("Restoring results");
        resDialog.setMessage("Select results to restore");
        
        resDialog.open();

        if (resDialog.getReturnCode() != resDialog.OK)
            return;

    }

    private void reportError(String info) {
        final IStatus executionStatus = new Status(IStatus.ERROR,
                Activator.PLUGIN_ID, info, null);
        StatusManager.getManager().handle(executionStatus,
                StatusManager.SHOW);
    }
    
    private ILabelProvider labelProvider = new LabelProvider() {

        @Override
        public Image getImage(Object element) {

            Image img = JavaUI
                    .getSharedImages()
                    .getImageDescriptor(
                            org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_CFILE)
                    .createImage();

            return img;
        }

        @Override
        public String getText(Object element) {
            if (!(element instanceof File))
                return null;
            File f = (File) element;

            SimpleDateFormat df = new SimpleDateFormat("yyyy.MM.dd HH:mm:ss");
            Date d = new Date();
            d.setTime(f.lastModified());

            log.debug(df.format(d));

            StringBuffer buf = new StringBuffer();
            buf.append(f.getName()).append(" (").append(df.format(d))
                    .append(")");
            return buf.toString();
        }

    };
    
}
