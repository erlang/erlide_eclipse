/*******************************************************************************
 * Copyright (c) 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.wizards;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.WorkbenchException;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.internal.WorkbenchPlugin;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.util.PluginUtils;
import org.erlide.runtime.ErlangProjectProperties;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.ui.perspectives.ErlangPerspective;

public class ErlangProjectImportWizard extends Wizard implements INewWizard { // IImportWizard
	// {
	private IWorkbench workbench;
	private IStructuredSelection selection;

	ErlangProjectImportWizardPage mainPage;

	// WizardFileSystemResourceImportPage1 mainPage;

	public ErlangProjectImportWizard() {
		AbstractUIPlugin plugin = WorkbenchPlugin.getDefault();
		IDialogSettings workbenchSettings = plugin.getDialogSettings();
		IDialogSettings section = workbenchSettings
				.getSection("FileSystemImportWizard");//$NON-NLS-1$
		if (section == null) {
			section = workbenchSettings.addNewSection("FileSystemImportWizard");//$NON-NLS-1$
		}
		setDialogSettings(section);
		// super();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.wizard.Wizard#performFinish()
	 */
	@Override
	public boolean performFinish() {
		if (!validateFinish()) {
			return false;
		}

		try {
			getContainer().run(false, true, new WorkspaceModifyOperation() {

				@Override
				protected void execute(IProgressMonitor monitor) {
					createProject((monitor != null) ? monitor
							: new NullProgressMonitor());

					try {
						final IWorkbench workbench = ErlideUIPlugin
								.getDefault().getWorkbench();
						workbench.showPerspective(ErlangPerspective.ID,
								workbench.getActiveWorkbenchWindow());
					} catch (final WorkbenchException we) {
						// ignore
					}
				}
			});
		} catch (final InvocationTargetException x) {
			reportError(x);
			return false;
		} catch (final InterruptedException x) {
			reportError(x);
			return false;
		}

		return mainPage.finish();
	}

	/**
	 * Validate finish
	 * 
	 * @return
	 */
	private boolean validateFinish() {

		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
	 *      org.eclipse.jface.viewers.IStructuredSelection)
	 */
	public void init(IWorkbench workbench, IStructuredSelection selection) {
		this.workbench = workbench;
		this.selection = selection;

		List<?> selectedResources = IDE.computeSelectedResources(selection);
		if (!selectedResources.isEmpty()) {
			this.selection = new StructuredSelection(selectedResources);
		}
		setWindowTitle("Erlang Project Import Wizard"); // NON-NLS-1
		setNeedsProgressMonitor(true);
		// mainPage = new ErlangProjectImportWizardPage("Import Erlang Project",
		// selection); // NON-NLS-1
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.wizard.IWizard#addPages()
	 */
	@Override
	public void addPages() {
		super.addPages();
		mainPage = new ErlangProjectImportWizardPage(workbench, selection);
		addPage(mainPage);

	}

	// /**
	// * Builds the path from the specified path list.
	// *
	// * @param monitor
	// * The progress monitor to use
	// * @param root
	// * the root worksapce
	// * @param project
	// * the project
	// * @param pathList
	// * the paths to create
	// * @throws CoreException
	// * if a problem occures
	// */
	// private void buildPaths(IProgressMonitor monitor, IWorkspaceRoot root,
	// IProject project) throws CoreException {
	// // Some paths are optionals (include): If we do not specify it, we get a
	// // null string and we do not need to create the directory
	// // if (null != pathList) {
	// final IPath projectPath = project.getFullPath();
	//
	// // String pathElement;
	// // for (String element : pathList) {
	// // pathElement = element;
	// // final IPath pp = new Path(pathElement);
	// // // only create in-project paths
	// // if (!pp.isAbsolute() && !".".equals(pathElement)) {
	// // final IPath path = projectPath.append(pathElement);
	// // final IFolder folder = root.getFolder(path);
	// // createFolderHelper(folder, monitor);
	// // }
	// // }
	// // }
	// }

	/**
	 * Displays an error that occured during the project creation. *
	 * 
	 * @param x
	 *            details on the error
	 */
	private void reportError(Exception x) {
		x.printStackTrace();
		ErrorDialog.openError(getShell(), ErlideUIPlugin
				.getResourceString("wizards.errors.projecterrordesc"),
				ErlideUIPlugin
						.getResourceString("wizards.errors.projecterrortitle"),
				PluginUtils.makeStatus(x));
	}

	// /**
	// * Displays an error that occured during the project creation. *
	// *
	// * @param x
	// * details on the error
	// */
	// private void reportError(String x) {
	// final Status status = new Status(IStatus.ERROR,
	// ErlideUIPlugin.PLUGIN_ID, 0, x, null);
	//
	// ErrorDialog.openError(getShell(), x, ErlideUIPlugin
	// .getResourceString("wizards.errors.projecterrortitle"), status);
	// }

	/**
	 * This is the actual implementation for project creation.
	 * 
	 * @param monitor
	 *            reports progress on this object
	 */
	protected void createProject(IProgressMonitor monitor) {
		monitor.beginTask(ErlideUIPlugin
				.getResourceString("wizards.messages.creatingproject"), 50);
		try {
			final IWorkspaceRoot root = ResourcesPlugin.getWorkspace()
					.getRoot();
			monitor.subTask(ErlideUIPlugin
					.getResourceString("wizards.messages.creatingdirectories"));
			final IProject project = root.getProject(mainPage.getProjectName());
			IProjectDescription description = ResourcesPlugin.getWorkspace()
					.newProjectDescription(project.getName());
			if (!Platform.getLocation().equals(mainPage.getLocationPath())) {
				description.setLocation(mainPage.getLocationPath());
			}
			project.create(description, monitor);
			monitor.worked(10);
			project.open(monitor);

			description = project.getDescription();

			description.setNatureIds(new String[] { ErlangPlugin.NATURE_ID });
			project.setDescription(description, new SubProgressMonitor(monitor,
					10));

			monitor.worked(10);
			monitor.subTask(ErlideUIPlugin
					.getResourceString("wizards.messages.creatingfiles"));

			// final ErlangProjectProperties bprefs = buildPage.getPrefs();

			// buildPaths(monitor, root, project);
			// buildPaths(monitor, root, project);
			// buildPaths(monitor, root, project);

			final ErlangProjectProperties prefs = new ErlangProjectProperties(
					project);
			// String[] directories = findErlDirectories();
			// prefs.setSourceDirs(directories);
			// monitor.worked(10);
			// directories = findHrlDirectories();
			// prefs.setIncludeDirs(directories);
			// prefs.copyFrom(bprefs);
			prefs.store();

			// add code path to backend
			final String out = project.getLocation().append(
					prefs.getOutputDir()).toString();
			BackendManager.getDefault().get(project).getCodeManager().addPath(
					prefs.getUsePathZ(), out);
		} catch (final CoreException x) {
			x.printStackTrace();
			reportError(x);
		} finally {
			monitor.done();
		}
	}

	private String[] findErlDirectories() {
		List<String> result = new ArrayList<String>();
		for (Iterator<?> iter = selection.iterator(); iter.hasNext();) {
			;
		}
		return result.toArray(new String[result.size()]);
	}

	private String[] findHrlDirectories() {
		List<String> result = new ArrayList<String>();
		for (Iterator<?> iter = selection.iterator(); iter.hasNext();) {
			;
		}
		return result.toArray(new String[result.size()]);
	}

	/**
	 * Helper method: it recursively creates a folder path.
	 * 
	 * @param folder
	 * @param monitor
	 * @throws CoreException
	 * @see java.io.File#mkdirs()
	 */
	private void createFolderHelper(IFolder folder, IProgressMonitor monitor)
			throws CoreException {
		if (!folder.exists()) {
			final IContainer parent = folder.getParent();
			if (parent instanceof IFolder && (!((IFolder) parent).exists())) {
				createFolderHelper((IFolder) parent, monitor);
			}

			folder.create(false, true, monitor);
		}
	}

}
