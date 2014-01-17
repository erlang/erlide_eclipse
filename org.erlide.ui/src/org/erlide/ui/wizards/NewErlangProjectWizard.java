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
import java.util.Map;

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
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.WorkbenchException;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.erlide.core.ErlangCore;
import org.erlide.core.internal.builder.ErlangNature;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.builder.BuilderConfigType;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.ui.ErlideUIConstants;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.perspectives.ErlangPerspective;
import org.erlide.util.ErlLogger;
import org.osgi.service.prefs.BackingStoreException;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

/**
 * Creates a new erlide project in the Eclipse workbench.
 * 
 * @author Eric Merritt [cyberlync at yahoo dot com]
 * @author Vlad Dumitrescu
 */
public class NewErlangProjectWizard extends Wizard implements INewWizard {

    private NewProjectData info;
    private Map<BuilderConfigType, ProjectPreferencesWizardPage> buildPages;
    private ErlangNewProjectCreationPage mainPage;
    private ErlangProjectBuilderPage builderPage;

    @Override
    public void init(final IWorkbench workbench, final IStructuredSelection selection) {
        setNeedsProgressMonitor(true);
    }

    @Override
    public void addPages() {
        try {
            super.addPages();
            info = new NewProjectData();
            buildPages = Maps.newEnumMap(BuilderConfigType.class);

            mainPage = new ErlangNewProjectCreationPage("mainPage", info);
            mainPage.setTitle(ErlideUIPlugin
                    .getResourceString("wizards.titles.newproject"));
            mainPage.setDescription(ErlideUIPlugin
                    .getResourceString("wizards.descs.newproject"));
            mainPage.setImageDescriptor(ErlideUIPlugin.getDefault().getImageDescriptor(
                    ErlideUIConstants.IMG_NEW_PROJECT_WIZARD));
            addPage(mainPage);

            builderPage = new ErlangProjectBuilderPage("builderPage", info);
            builderPage.setTitle(ErlideUIPlugin
                    .getResourceString("wizards.titles.builderprefs"));
            builderPage.setDescription(ErlideUIPlugin
                    .getResourceString("wizards.descs.builderprefs"));
            builderPage.setImageDescriptor(ErlideUIPlugin.getDefault()
                    .getImageDescriptor(ErlideUIConstants.IMG_NEW_PROJECT_WIZARD));
            addPage(builderPage);

            for (final BuilderConfigType builder : BuilderConfigType.values()) {
                final ProjectPreferencesWizardPage buildPage = ProjectPreferencesWizardPageFactory
                        .create(builder, info);
                buildPages.put(builder, buildPage);
                buildPage.setImageDescriptor(ErlideUIPlugin.getDefault()
                        .getImageDescriptor(ErlideUIConstants.IMG_NEW_PROJECT_WIZARD));
                addPage(buildPage);

            }
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
                protected void execute(final IProgressMonitor monitor)
                        throws InvocationTargetException {
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
            final IProject project = root.getProject(info.getName());
            IProjectDescription description = ResourcesPlugin.getWorkspace()
                    .newProjectDescription(project.getName());
            if (!Platform.getLocation().equals(info.getLocation())) {
                description.setLocation(info.getLocation());
            }
            project.create(description, monitor);
            monitor.worked(10);
            project.open(monitor);

            description = project.getDescription();
            description.setNatureIds(new String[] { ErlangCore.NATURE_ID });
            project.setDescription(description, new SubProgressMonitor(monitor, 10));
            if (info.getBuilder() != null) {
                ErlangNature.setErlangProjectBuilder(project, info.getBuilder());
                createBuilderConfig(info.getBuilder());
            }

            monitor.worked(10);
            monitor.subTask(ErlideUIPlugin
                    .getResourceString("wizards.messages.creatingfiles"));

            createFolders(project, Lists.newArrayList(info.getOutputDir()), monitor);
            createFolders(project, info.getSourceDirs(), monitor);
            createFolders(project, info.getIncludeDirs(), monitor);

            final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                    .getErlangProject(project);
            erlProject.setProperties(info);
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

    private void createBuilderConfig(final BuilderTool builderTool) {
        // BuilderInfo.valueOf(builderName).getBuilder().setConfiguration(info);
    }

    /**
     * Builds the path from the specified path list.
     */
    private void createFolders(final IProject project, final Collection<IPath> pathList,
            final IProgressMonitor monitor) throws CoreException {
        // Some paths are optional (include): If we do not specify it, we get a
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

    private void reportError(final Exception x) {
        ErlLogger.error(x);
        final String description = ErlideUIPlugin
                .getResourceString("wizards.errors.projecterrordesc");
        final String title = ErlideUIPlugin
                .getResourceString("wizards.errors.projecterrortitle");
        ErrorDialog.openError(getShell(), description, title, new Status(IStatus.ERROR,
                ErlideUIPlugin.PLUGIN_ID, 0, x.getMessage(), x));
    }

    private void reportError(final String x) {
        final Status status = new Status(IStatus.ERROR, ErlideUIPlugin.PLUGIN_ID, 0, x,
                null);
        final String title = ErlideUIPlugin
                .getResourceString("wizards.errors.projecterrortitle");
        ErrorDialog.openError(getShell(), x, title, status);
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

    @Override
    public IWizardPage getNextPage(final IWizardPage page) {
        if (page == mainPage) {
            return builderPage;
        }
        if (page == builderPage) {
            final BuilderConfigType config;
            if (info.getBuilder().equals(BuilderTool.MAKE)
                    || info.getBuilder().equals(BuilderTool.INTERNAL)) {
                config = info.getBuilderConfig();
            } else {
                config = info.getBuilder().getMatchingConfigs().iterator().next();
            }
            return buildPages.get(config);
        }
        return null;
    }
}
