package org.erlide.ui.properties;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.dialogs.ISelectionStatusValidator;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.eclipse.ui.views.navigator.ResourceComparator;
import org.erlide.ui.util.FolderSelectionDialog;
import org.erlide.ui.util.StatusInfo;
import org.erlide.ui.util.TypedElementSelectionValidator;
import org.erlide.ui.util.TypedViewerFilter;

public class ProjectDirectoryFieldEditor extends DirectoryFieldEditor {
	private final IProject project;
	private String fOutputLocation;

	public ProjectDirectoryFieldEditor(String name, String labelText,
			Composite parent, IProject project) {
		super(name, labelText, parent);
		this.project = project;
	}

	@Override
	protected String changePressed() {
		IContainer container = chooseLocation();
		if (container != null) {
			return container.getProjectRelativePath().toString();
		}
		return null;
	}

	@Override
	protected boolean doCheckState() {
		String fileName = getTextControl().getText();
		fileName = fileName.trim();
		if (fileName.length() == 0 && isEmptyStringAllowed()) {
			return true;
		}
		if (project != null) {
			String prjLoc = project.getLocation().toString();
			fileName = prjLoc + "/" + fileName;
		}
		File file = new File(fileName);
		return file.isDirectory();
	}

	private IContainer chooseLocation() {
		IWorkspaceRoot root = project.getWorkspace().getRoot();
		final Class<?>[] acceptedClasses = new Class[] { IProject.class,
				IFolder.class };
		IProject[] allProjects = root.getProjects();
		List<IProject> rejectedElements = new ArrayList<IProject>(
				allProjects.length);
		for (int i = 0; i < allProjects.length; i++) {
			if (!allProjects[i].equals(project)) {
				rejectedElements.add(allProjects[i]);
			}
		}
		ViewerFilter filter = new TypedViewerFilter(acceptedClasses,
				rejectedElements.toArray());

		ILabelProvider lp = new WorkbenchLabelProvider();
		ITreeContentProvider cp = new WorkbenchContentProvider();

		IResource initSelection = null;
		if (fOutputLocation != null) {
			initSelection = root.findMember(fOutputLocation);
		}

		FolderSelectionDialog dialog = new FolderSelectionDialog(getShell(),
				lp, cp);
		dialog.setTitle("Choose folder");

		ISelectionStatusValidator validator = new ISelectionStatusValidator() {
			ISelectionStatusValidator validator = new TypedElementSelectionValidator(
					acceptedClasses, false);

			public IStatus validate(Object[] selection) {
				IStatus typedStatus = validator.validate(selection);
				if (!typedStatus.isOK()) {
					return typedStatus;
				}
				if (selection[0] instanceof IFolder) {
					IFolder folder = (IFolder) selection[0];
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
				} else {
					return new StatusInfo(IStatus.ERROR, "");
				}
			}
		};
		dialog.setValidator(validator);
		dialog.setMessage(getLabelText());
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
