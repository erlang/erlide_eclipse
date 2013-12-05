package org.erlide.ui.properties;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ISelectionStatusValidator;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.eclipse.ui.views.navigator.ResourceComparator;
import org.erlide.ui.util.FolderSelectionDialog;
import org.erlide.ui.util.StatusInfo;
import org.erlide.ui.util.TypedElementSelectionValidator;
import org.erlide.ui.util.TypedViewerFilter;

public class DirectorySelectUtil {

    public static IContainer chooseLocation(final String dialogTitle,
            final String labelText, final IProject project2, final String outputLocation,
            final Shell shell) {
        final IWorkspaceRoot root = project2.getWorkspace().getRoot();
        final Class<?>[] acceptedClasses = new Class[] { IProject.class, IFolder.class };
        final IProject[] allProjects = root.getProjects();
        final List<IProject> rejectedElements = new ArrayList<IProject>(
                allProjects.length);
        for (int i = 0; i < allProjects.length; i++) {
            if (!allProjects[i].equals(project2)) {
                rejectedElements.add(allProjects[i]);
            }
        }
        final ViewerFilter filter = new TypedViewerFilter(acceptedClasses,
                rejectedElements.toArray());

        final ILabelProvider lp = new WorkbenchLabelProvider();
        final ITreeContentProvider cp = new WorkbenchContentProvider();

        IResource initSelection = null;
        if (outputLocation != null) {
            initSelection = root.findMember(outputLocation);
        }

        final FolderSelectionDialog dialog = new FolderSelectionDialog(shell, lp, cp);
        dialog.setTitle(dialogTitle);

        final ISelectionStatusValidator validator = new ISelectionStatusValidator() {
            ISelectionStatusValidator myValidator = new TypedElementSelectionValidator(
                    acceptedClasses, false);

            @Override
            public IStatus validate(final Object[] selection) {
                final IStatus typedStatus = myValidator.validate(selection);
                if (!typedStatus.isOK()) {
                    return typedStatus;
                }
                if (selection[0] instanceof IFolder) {
                    // try {
                    // IStatus result = ClasspathModifier
                    // .checkSetOutputLocationPrecondition(
                    // fEntryToEdit, folder.getFullPath(),
                    // fAllowInvalidClasspath, fCPJavaProject);
                    // if (result.getSeverity() == IStatus.ERROR) {
                    // return result;
                    // }
                    // } catch (CoreException e) {
                    // JavaPlugin.log(e);
                    // }
                    return new StatusInfo();
                }
                return new StatusInfo(IStatus.ERROR, "");
            }
        };
        dialog.setValidator(validator);
        dialog.setMessage(labelText);
        dialog.addFilter(filter);
        dialog.setInput(root);
        dialog.setInitialSelection(initSelection);
        dialog.setComparator(new ResourceComparator(ResourceComparator.NAME));

        if (dialog.open() == Window.OK) {
            return (IContainer) dialog.getFirstResult();
        }
        return null;
    }

}
