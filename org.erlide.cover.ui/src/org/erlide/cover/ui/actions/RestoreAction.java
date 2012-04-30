package org.erlide.cover.ui.actions;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.ObjectInputStream;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;

import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.cover.core.Logger;
import org.erlide.cover.core.MD5Checksum;
import org.erlide.cover.ui.Activator;
import org.erlide.cover.ui.CoverageHelper;
import org.erlide.cover.ui.Images;
import org.erlide.cover.ui.annotations.EditorTracker;
import org.erlide.cover.views.model.ICoverageObject;
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

    private final Shell shell;
    private final TreeViewer viewer;

    private final Logger log; // logger

    public RestoreAction(final TreeViewer viewer) {
        shell = viewer.getControl().getShell();
        this.viewer = viewer;
        log = Activator.getDefault();
    }

    @Override
    public void run() {

        final IPath location = Activator.getDefault().getStateLocation()
                .append(SaveAction.DIR_NAME);
        final File dir = location.toFile();

        if (!dir.exists() && !dir.mkdir()) {
            CoverageHelper.reportError("Can not save results!");
            return;
        }

        // open dialog
        final ElementListSelectionDialog resDialog = new ElementListSelectionDialog(
                shell, labelProvider);

        resDialog.setElements(dir.listFiles());
        resDialog.setTitle("Restoring results");
        resDialog.setMessage("Select results to restore");

        resDialog.open();

        if (resDialog.getReturnCode() != Window.OK) {
            return;
        }
        //
        final File f = (File) resDialog.getFirstResult();

        try {
            final ObjectInputStream objStream = new ObjectInputStream(
                    new FileInputStream(f));

            final Object obj = objStream.readObject();

            StatsTreeModel.changeInstance((StatsTreeModel) obj);
            StatsTreeModel.getInstance().setChanged(true);

            viewer.setInput(StatsTreeModel.getInstance());

            final ICoverageObject root = StatsTreeModel.getInstance().getRoot();
            final ModuleSet mSet = new ModuleSet();
            createModuleSet(mSet, root);

            final Collection<ICoverageObject> col = root.getModules();
            for (final ICoverageObject module : col) {
                if (ifMarkAnnotations((ModuleStats) module)) {
                    ((ModuleStats) module).couldBeMarked = true;
                } else {
                    ((ModuleStats) module).couldBeMarked = false;
                }
            }
            EditorTracker.getInstance().addAnnotations();

        } catch (final FileNotFoundException e) {
            log.error("No such file");
            e.printStackTrace();
            CoverageHelper.reportError("Error while reading file");
        } catch (final Exception e) {
            log.error("Error while reading file");
            e.printStackTrace();
            CoverageHelper.reportError("Error while reading file");
        }

    }

    // creates module set used to prepare annotations map
    private void createModuleSet(final ModuleSet mSet,
            final ICoverageObject object) {
        if (object.getType().equals(ObjectType.MODULE)) {
            ModuleSet.add((ModuleStats) object);
        }
        final ICoverageObject[] children = object.getChildren();
        for (final ICoverageObject child : children) {
            createModuleSet(mSet, child);
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

    // label provider for choosing files
    private final ILabelProvider labelProvider = new LabelProvider() {

        @Override
        public Image getImage(final Object element) {

            final Image img = Activator.getImageDescriptor(Images.RAW_FILE)
                    .createImage();
            return img;
        }

        @Override
        public String getText(final Object element) {
            if (!(element instanceof File)) {
                return null;
            }
            final File f = (File) element;

            final SimpleDateFormat df = new SimpleDateFormat(
                    "yyyy.MM.dd HH:mm:ss");
            final Date d = new Date();
            d.setTime(f.lastModified());

            log.info(df.format(d));

            final StringBuffer buf = new StringBuffer();
            buf.append(f.getName()).append(" (").append(df.format(d))
                    .append(")");
            return buf.toString();
        }

    };

}
