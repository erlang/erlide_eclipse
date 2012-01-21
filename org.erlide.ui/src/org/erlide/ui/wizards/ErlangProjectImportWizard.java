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
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.IPageChangeProvider;
import org.eclipse.jface.dialogs.IPageChangedListener;
import org.eclipse.jface.dialogs.PageChangedEvent;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IImportWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.WorkbenchException;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.eclipse.ui.dialogs.FileSystemElement;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.wizards.datatransfer.FileSystemStructureProvider;
import org.erlide.backend.BackendCore;
import org.erlide.core.ErlangCore;
import org.erlide.core.internal.model.root.OldErlangProjectProperties;
import org.erlide.core.model.util.PluginUtils;
import org.erlide.jinterface.ErlLogger;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.perspectives.ErlangPerspective;

public class ErlangProjectImportWizard extends Wizard implements IImportWizard {
    // {
    private IStructuredSelection selection;
    private ErlangProjectImportWizardPage mainPage;
    private ErlangProjectImportIncludeAndSourceDirsWizardPage importIncludeAndSourceDirsPage;
    private Collection<String> resources;

    public static boolean COPY_ONLY = !true;

    // WizardFileSystemResourceImportPage1 mainPage;

    public ErlangProjectImportWizard() {
        super();
        final IDialogSettings workbenchSettings = ErlideUIPlugin.getDefault()
                .getDialogSettings();
        IDialogSettings section = workbenchSettings
                .getSection("FileSystemImportWizard");//$NON-NLS-1$
        if (section == null) {
            section = workbenchSettings.addNewSection("FileSystemImportWizard");//$NON-NLS-1$
        }
        setDialogSettings(section);
    }

    public void setupDirectoriesPage() {
        final List<?> selectedResources = mainPage.getSelectedResources();
        final List<String> filesAndDirs = new ArrayList<String>(
                selectedResources.size());
        final FileSystemStructureProvider provider = FileSystemStructureProvider.INSTANCE;
        for (final Object o : selectedResources) {
            final FileSystemElement fse = (FileSystemElement) o;
            final Object fso = fse.getFileSystemObject();
            final String s = provider.getFullPath(fso);
            filesAndDirs.add(s);
        }
        final String projectPath = mainPage.getProjectPath().toString();
        final ErlProjectImport epi = ErlideImport
                .importProject(BackendCore.getBackendManager().getIdeBackend(),
                        projectPath, filesAndDirs);
        final IPath beamDir = new Path(epi.getBeamDir());
        importIncludeAndSourceDirsPage.setup(projectPath, epi.getDirectories(),
                epi.getIncludeDirs(), epi.getSourceDirs(), beamDir);
        resources = epi.getResources();
        final List<Object> fileSystemObjects = new ArrayList<Object>();
        for (final Object o : selectedResources) {
            final FileSystemElement fse = (FileSystemElement) o;
            final Object fso = fse.getFileSystemObject();
            final String s = provider.getFullPath(fso);
            if (epi.getResources().contains(s)) {
                fileSystemObjects.add(((FileSystemElement) o)
                        .getFileSystemObject());
            }
        }
    }

    @Override
    public boolean performFinish() {
        if (!validateFinish()) {
            return false;
        }
        if (resources == null) {
            setupDirectoriesPage();
        }
        final List<?> selectedResources = mainPage.getSelectedResources();
        final List<String> filesAndDirs = new ArrayList<String>(
                selectedResources.size());
        final FileSystemStructureProvider provider = FileSystemStructureProvider.INSTANCE;
        for (final Object o : selectedResources) {
            final FileSystemElement fse = (FileSystemElement) o;
            final Object fso = fse.getFileSystemObject();
            final String s = provider.getFullPath(fso);
            filesAndDirs.add(s);
        }
        final List<Object> fileSystemObjects = new ArrayList<Object>();
        for (final Object o : selectedResources) {
            final FileSystemElement fse = (FileSystemElement) o;
            final Object fso = fse.getFileSystemObject();
            final String s = provider.getFullPath(fso);
            if (resources.contains(s)) {
                fileSystemObjects.add(((FileSystemElement) o)
                        .getFileSystemObject());
            }
        }
        try {
            getContainer().run(false, true, new WorkspaceModifyOperation() {

                @Override
                protected void execute(IProgressMonitor monitor) {
                    if (monitor == null) {
                        monitor = new NullProgressMonitor();
                    }
                    try {
                        createProject(
                                monitor,
                                mainPage.getProjectPath(),
                                mainPage.getProjectName(),
                                importIncludeAndSourceDirsPage.getIncludeDirs(),
                                importIncludeAndSourceDirsPage.getSourceDirs(),
                                importIncludeAndSourceDirsPage.getEbinDir());
                    } catch (final InvocationTargetException e) {
                        reportError(e);
                    }

                    try {
                        final IWorkbench wbench = ErlideUIPlugin.getDefault()
                                .getWorkbench();
                        wbench.showPerspective(ErlangPerspective.ID,
                                wbench.getActiveWorkbenchWindow());
                    } catch (final WorkbenchException we) {
                        // ignore
                    }
                }
            });

        } catch (final Exception e) {
            reportError(e);
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

        return true;
    }

    @Override
    public void init(final IWorkbench aWorkbench,
            final IStructuredSelection aSelection) {
        selection = aSelection;

        final List<?> selectedResources = IDE
                .computeSelectedResources(aSelection);
        if (!selectedResources.isEmpty()) {
            selection = new StructuredSelection(selectedResources);
        }
        setWindowTitle("Erlang Project Import Wizard"); // NON-NLS-1
        setNeedsProgressMonitor(true);
        // mainPage = new ErlangProjectImportWizardPage("Import Erlang Project",
        // selection); // NON-NLS-1
    }

    @Override
    public void addPages() {
        super.addPages();
        mainPage = new ErlangProjectImportWizardPage(selection);
        addPage(mainPage);
        importIncludeAndSourceDirsPage = new ErlangProjectImportIncludeAndSourceDirsWizardPage();
        addPage(importIncludeAndSourceDirsPage);
    }

    @Override
    public void setContainer(final IWizardContainer wizardContainer) {
        if (wizardContainer instanceof IPageChangeProvider) {
            final IPageChangeProvider pcp = (IPageChangeProvider) wizardContainer;
            pcp.addPageChangedListener(new IPageChangedListener() {

                @Override
                public void pageChanged(final PageChangedEvent event) {
                    if (event.getSelectedPage() == importIncludeAndSourceDirsPage) {
                        setupDirectoriesPage();
                    }
                }
            });
        }
        super.setContainer(wizardContainer);
    }

    /**
     * Displays an error that occured during the project creation. *
     * 
     * @param e
     *            details on the error
     */
    private void reportError(final Exception e) {
        ErlLogger.error(e);
        ErrorDialog.openError(getShell(), ErlideUIPlugin
                .getResourceString("wizards.errors.projecterrordesc"),
                ErlideUIPlugin
                        .getResourceString("wizards.errors.projecterrortitle"),
                PluginUtils.makeStatus(e));
    }

    protected boolean createProject(final IProgressMonitor monitor,
            final IPath projectLocation, final String projectName,
            final Collection<IPath> includeDirs,
            final Collection<IPath> sourceDirs, final IPath outputDir)
            throws InvocationTargetException {
        final IWorkspace workspace = ResourcesPlugin.getWorkspace();
        final IProject project = workspace.getRoot().getProject(projectName);
        final IProjectDescription description = workspace
                .newProjectDescription(projectName);
        description.setLocation(projectLocation);

        final ICommand[] old = description.getBuildSpec(), specs = new ICommand[old.length + 1];
        System.arraycopy(old, 0, specs, 0, old.length);
        final ICommand command = description.newCommand();
        command.setBuilderName(ErlangCore.BUILDER_ID);
        specs[old.length] = command;
        description.setBuildSpec(specs);
        description.setNatureIds(new String[] { ErlangCore.NATURE_ID });

        try {
            monitor.beginTask(
                    WizardMessages.WizardProjectsImportPage_CreateProjectsTask,
                    1000);
            project.create(description, monitor);
            // final int subTicks = 600 / fileSystemObjects.size();
            // createLinks(theProjectPath, fileSystemObjects, monitor, project,
            // subTicks);
            // project.create(description, IResource.REPLACE,
            // new SubProgressMonitor(monitor, 30));
            project.open(IResource.BACKGROUND_REFRESH, new SubProgressMonitor(
                    monitor, 300));
            final OldErlangProjectProperties erlangProjectProperties = new OldErlangProjectProperties(
                    project);
            erlangProjectProperties.setIncludeDirs(includeDirs);
            erlangProjectProperties.setSourceDirs(sourceDirs);
            erlangProjectProperties.setOutputDir(outputDir);
        } catch (final CoreException e) {
            throw new InvocationTargetException(e);
        } finally {
            monitor.done();
        }
        return true;

    }
}
