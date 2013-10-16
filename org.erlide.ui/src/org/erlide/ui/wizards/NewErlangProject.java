/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.wizards;

import java.lang.reflect.InvocationTargetException;
import java.util.Collection;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.WorkbenchException;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.erlide.core.ErlangCore;
import org.erlide.core.internal.builder.ErlangNature;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.IErlangProjectProperties;
import org.erlide.ui.ErlideUIConstants;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.perspectives.ErlangPerspective;
import org.erlide.util.ErlLogger;
import org.osgi.service.prefs.BackingStoreException;

import com.google.common.collect.Lists;

/**
 * Creates a new erlide project in the Eclipse workbench.
 * 
 * @author Eric Merritt [cyberlync at yahoo dot com]
 * @author Vlad Dumitrescu
 */
public class NewErlangProject extends Wizard implements INewWizard {

    private static final String MAIN_PAGE = "mainPage";
    private static final String BUILD_PAGE = "buildPage";

    private WizardNewProjectCreationPage mainPage;
    private ProjectPreferencesWizardPage buildPage;
    private IErlangProjectProperties info;

    @Override
    public void init(final IWorkbench workbench, final IStructuredSelection selection) {
        setNeedsProgressMonitor(true);
    }

    @Override
    public void addPages() {
        try {
            super.addPages();
            info = new ErlangProjectProperties();
            mainPage = new ProjectNewCreationPage(MAIN_PAGE, info);
            mainPage.setTitle(ErlideUIPlugin
                    .getResourceString("wizards.titles.newproject"));
            mainPage.setDescription(ErlideUIPlugin
                    .getResourceString("wizards.descs.newproject"));
            mainPage.setImageDescriptor(ErlideUIPlugin.getDefault().getImageDescriptor(
                    ErlideUIConstants.IMG_NEW_PROJECT_WIZARD));
            addPage(mainPage);

            buildPage = new ProjectPreferencesWizardPage(BUILD_PAGE, info);
            buildPage.setTitle(ErlideUIPlugin
                    .getResourceString("wizards.titles.buildprefs"));
            buildPage.setDescription(ErlideUIPlugin
                    .getResourceString("wizards.descs.buildprefs"));
            buildPage.setImageDescriptor(ErlideUIPlugin.getDefault().getImageDescriptor(
                    ErlideUIConstants.IMG_NEW_PROJECT_WIZARD));
            addPage(buildPage);
        } catch (final Exception x) {
            reportError(x);
        }
    }

    /**
     * User has clicked "Finish", we create the project. In practice, it calls
     * the createProject() method in the appropriate thread.
     * 
     * @see org.eclipse.jface.wizard.IWizard#performFinish()
     */
    @Override
    public boolean performFinish() {
        if (!validateFinish()) {
            return false;
        }

        try {
            getContainer().run(false, true, new WorkspaceModifyOperation() {

                @Override
                protected void execute(final IProgressMonitor monitor) {
                    createProject(monitor != null ? monitor : new NullProgressMonitor());

                    try {
                        final IWorkbench workbench = ErlideUIPlugin.getDefault()
                                .getWorkbench();
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

        return true;
    }

    /**
     * Validate finish
     * 
     * @return
     */
    private boolean validateFinish() {
        if (info.getOutputDir().isEmpty()) {
            reportError(ErlideUIPlugin.getResourceString("wizard.errors.buildpath"));
            return false;
        }

        if (info.getSourceDirs().isEmpty()) {
            reportError(ErlideUIPlugin.getResourceString("wizards.errors.sourcepath"));
            return false;
        }
        return true;
    }

    protected void createProject(final IProgressMonitor monitor) {
        monitor.beginTask(
                ErlideUIPlugin.getResourceString("wizards.messages.creatingproject"), 50);
        try {
            final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
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
            description.setNatureIds(new String[] { ErlangCore.NATURE_ID });
            project.setDescription(description, new SubProgressMonitor(monitor, 10));
            if (info.getBuilderName() != null) {
                ErlangNature.setErlangProjectBuilder(project, info.getBuilderName());
                createBuilderConfig(project, info.getBuilderName());
            }

            monitor.worked(10);
            monitor.subTask(ErlideUIPlugin
                    .getResourceString("wizards.messages.creatingfiles"));

            createFolders(project, Lists.newArrayList(info.getOutputDir()), monitor);
            createFolders(project, info.getSourceDirs(), monitor);
            createFolders(project, info.getIncludeDirs(), monitor);

            final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                    .getErlangProject(project);
            erlProject.setAllProperties(info);
        } catch (final CoreException e) {
            ErlLogger.debug(e);
            reportError(e);
        } catch (final BackingStoreException e) {
            ErlLogger.debug(e);
            reportError(e);
        } finally {
            monitor.done();
        }
    }

    private void createBuilderConfig(final IProject project, final String builderName) {
        ErlangNature.getBuilder(builderName).createConfig(project.getLocation(), info);
    }

    /**
     * Builds the path from the specified path list.
     */
    private void createFolders(final IProject project, final Collection<IPath> pathList,
            final IProgressMonitor monitor) throws CoreException {
        // Some paths are optionals (include): If we do not specify it, we get a
        // null string and we do not need to create the directory
        if (pathList != null) {
            for (final IPath path : pathList) {
                // only create in-project paths
                if (!path.isAbsolute() && !path.toString().equals(".") && !path.isEmpty()) {
                    final IFolder folder = project.getFolder(path);
                    createFolderHelper(folder, monitor);
                }
            }
        }
    }

    /**
     * Displays an error that occured during the project creation.
     */
    private void reportError(final Exception x) {
        ErlLogger.error(x);
        ErrorDialog
                .openError(
                        getShell(),
                        ErlideUIPlugin
                                .getResourceString("wizards.errors.projecterrordesc"),
                        ErlideUIPlugin
                                .getResourceString("wizards.errors.projecterrortitle"),
                        new Status(IStatus.ERROR, ErlideUIPlugin.PLUGIN_ID, 0, x
                                .getMessage(), x));
    }

    /**
     * Displays an error that occured during the project creation. *
     * 
     * @param x
     *            details on the error
     */
    private void reportError(final String x) {
        final Status status = new Status(IStatus.ERROR, ErlideUIPlugin.PLUGIN_ID, 0, x,
                null);

        ErrorDialog.openError(getShell(), x,
                ErlideUIPlugin.getResourceString("wizards.errors.projecterrortitle"),
                status);
    }

    /**
     * Helper method: it recursively creates a folder path.
     * 
     * @param folder
     * @param monitor
     * @throws CoreException
     * @see java.io.File#mkdirs()
     */
    private void createFolderHelper(final IFolder folder, final IProgressMonitor monitor)
            throws CoreException {
        if (!folder.exists()) {
            final IContainer parent = folder.getParent();
            if (parent instanceof IFolder && !((IFolder) parent).exists()) {
                createFolderHelper((IFolder) parent, monitor);
            }

            folder.create(false, true, monitor);
        }
    }

}
