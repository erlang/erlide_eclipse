package org.erlide.cover.ui.actions;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.ObjectInputStream;
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
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.eclipse.ui.statushandlers.StatusManager;
import org.erlide.cover.ui.Activator;
import org.erlide.cover.ui.Images;
import org.erlide.cover.ui.annotations.EditorTracker;
import org.erlide.cover.views.model.ICoverageObject;
import org.erlide.cover.views.model.IStatsTreeObject;
import org.erlide.cover.views.model.ModuleSet;
import org.erlide.cover.views.model.ModuleStats;
import org.erlide.cover.views.model.ObjectType;
import org.erlide.cover.views.model.StatsTreeModel;

/**
 * An action for restoring coverage results which were previously saved
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions com>
 * 
 */
public class RestoreAction extends Action {

    private Shell shell;
    private TreeViewer viewer;

    private Logger log; // logger

    public RestoreAction(TreeViewer viewer) {
        this.shell = viewer.getControl().getShell();
        this.viewer = viewer;
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
     //  
        File f = (File)resDialog.getFirstResult();
        
        try {
            ObjectInputStream objStream = new ObjectInputStream(new FileInputStream(f));
            
            Object obj = objStream.readObject();
            
            StatsTreeModel.changeInstance((StatsTreeModel)obj);
            
            viewer.setInput(StatsTreeModel.getInstance());
            
            ICoverageObject root = StatsTreeModel.getInstance().getRoot();
            ModuleSet mSet = new ModuleSet();
            createModuleSet(mSet, root);
            
            EditorTracker.getInstance().addAnnotations();
            
        } catch (FileNotFoundException e) {
            log.error("No such file");
            e.printStackTrace();
            reportError("Error while reading file");
        } catch (Exception e) {
            log.error("Error while reading file");
            e.printStackTrace();
            reportError("Error while reading file");
        } 
        
    }

    // creates module set used to prepare annotations map
    private void createModuleSet(ModuleSet mSet, IStatsTreeObject object) {
        if(object.getType().equals(ObjectType.MODULE))
            ModuleSet.add((ModuleStats)object);
        IStatsTreeObject[] children = object.getChildren();
        for(IStatsTreeObject child : children)
            createModuleSet(mSet, child);
    }

    // reports error to a user
    private void reportError(String info) {
        final IStatus executionStatus = new Status(IStatus.ERROR,
                Activator.PLUGIN_ID, info, null);
        StatusManager.getManager().handle(executionStatus,
                StatusManager.SHOW);
    }
    
    // label provider for choosing files
    private ILabelProvider labelProvider = new LabelProvider() {

        @Override
        public Image getImage(Object element) {

            Image img = Activator.getImageDescriptor(Images.RAW_FILE).createImage();
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
