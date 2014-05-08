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

import java.net.URI;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.dialogs.WizardNewProjectReferencePage;
import org.eclipse.ui.ide.undo.WorkspaceUndoUtil;
import org.eclipse.ui.part.ISetSelectionTarget;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.NewProjectData;
import org.erlide.engine.model.root.ProjectConfigType;
import org.erlide.ui.ErlideUIConstants;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.util.ErlLogger;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

/**
 * Creates a new erlang project in the Eclipse workbench.
 *
 * @author Eric Merritt [cyberlync at yahoo dot com]
 * @author Vlad Dumitrescu
 */
public class NewErlangProjectWizard extends Wizard implements INewWizard {

    private IWorkbench workbench;

    private NewProjectData info;
    private Map<ProjectConfigType, ProjectPreferencesWizardPage> buildPages;
    private ErlangNewProjectCreationPage mainPage;
    private ErlangProjectBuilderPage builderPage;
    private WizardNewProjectReferencePage referencePage;

    private IWizardPage prevPage;

    @Override
    public void init(final IWorkbench aWorkbench, final IStructuredSelection selection) {
        workbench = aWorkbench;
        setNeedsProgressMonitor(true);
    }

    @Override
    public void addPages() {
        try {
            super.addPages();
            info = new NewProjectData(ErlangEngine.getInstance()
                    .getProjectConfiguratorFactory());
            info.copyFrom(ErlangProjectProperties.DEFAULT);

            buildPages = Maps.newEnumMap(ProjectConfigType.class);

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

            for (final ProjectConfigType builder : ProjectConfigType.values()) {
                final ProjectPreferencesWizardPage buildPage = ProjectPreferencesWizardPageFactory
                        .create(builder, info);
                buildPages.put(builder, buildPage);
                buildPage.setImageDescriptor(ErlideUIPlugin.getDefault()
                        .getImageDescriptor(ErlideUIConstants.IMG_NEW_PROJECT_WIZARD));
                addPage(buildPage);

            }

            // only add page if there are already projects in the workspace
            if (ResourcesPlugin.getWorkspace().getRoot().getProjects().length > 0) {
                referencePage = new WizardNewProjectReferencePage(
                        "basicReferenceProjectPage");//$NON-NLS-1$
                referencePage.setTitle(WizardMessages.NewProject_referenceTitle);
                referencePage
                        .setDescription(WizardMessages.NewProject_referenceDescription);
                addPage(referencePage);
            }

        } catch (final Exception x) {
            reportError(x);
        }
    }

    @Override
    public boolean performFinish() {
        System.out.println("CREATE " + info);
        IProject newProject;
        try {
            newProject = createNewProject();
        } catch (final CoreException e) {
            System.out.println(">>> project exists!");
            return false;
        }

        if (newProject == null) {
            return false;
        }

        updatePerspective();
        selectAndReveal(newProject);

        return true;
    }

    private IProject createNewProject() throws CoreException {
        URI location = null;
        if (!mainPage.useDefaults()) {
            location = mainPage.getLocationURI();
        }

        IProject[] refProjects = null;
        if (referencePage != null) {
            refProjects = referencePage.getReferencedProjects();
        }

        final ProjectCreator creator = new ProjectCreator(mainPage.getProjectName(),
                location, refProjects, info, getContainer(),
                WorkspaceUndoUtil.getUIInfoAdapter(getShell()));
        return creator.createProject();

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

    @Override
    public IWizardPage getNextPage(final IWizardPage page) {
        if (page == mainPage) {
            return builderPage;
        }
        if (page == builderPage) {
            final ProjectConfigType config;
            if (info.getBuilder().equals(BuilderTool.MAKE)
                    || info.getBuilder().equals(BuilderTool.INTERNAL)) {
                config = info.getConfigType();
            } else {
                config = info.getBuilder().getMatchingConfigs().iterator().next();
            }
            return buildPages.get(config);
        }
        if (buildPages.containsValue(page)) {
            prevPage = page;
            return referencePage;
        }
        return null;
    }

    @Override
    public IWizardPage getPreviousPage(final IWizardPage page) {
        if (page == builderPage) {
            return mainPage;
        }
        if (page == referencePage) {
            return prevPage;
        }
        if (buildPages.containsValue(page)) {
            return builderPage;
        }
        return null;
    }

    /**
     * Selects and reveals the newly added resource in all parts of the active
     * workbench window's active page.
     *
     * @see ISetSelectionTarget
     */
    protected void selectAndReveal(final IResource newResource) {
        selectAndReveal(newResource, getWorkbench().getActiveWorkbenchWindow());
    }

    /**
     * Attempts to select and reveal the specified resource in all parts within
     * the supplied workbench window's active page.
     * <p>
     * Checks all parts in the active page to see if they implement
     * <code>ISetSelectionTarget</code>, either directly or as an adapter. If
     * so, tells the part to select and reveal the specified resource.
     * </p>
     *
     * @param resource
     *            the resource to be selected and revealed
     * @param window
     *            the workbench window to select and reveal the resource
     *
     * @see ISetSelectionTarget
     */
    public static void selectAndReveal(final IResource resource,
            final IWorkbenchWindow window) {
        // validate the input
        if (window == null || resource == null) {
            return;
        }
        final IWorkbenchPage page = window.getActivePage();
        if (page == null) {
            return;
        }

        // get all the view and editor parts
        final List<IWorkbenchPart> parts = Lists.newArrayList();
        IWorkbenchPartReference refs[] = page.getViewReferences();
        for (int i = 0; i < refs.length; i++) {
            final IWorkbenchPart part = refs[i].getPart(false);
            if (part != null) {
                parts.add(part);
            }
        }
        refs = page.getEditorReferences();
        for (int i = 0; i < refs.length; i++) {
            if (refs[i].getPart(false) != null) {
                parts.add(refs[i].getPart(false));
            }
        }

        final ISelection selection = new StructuredSelection(resource);
        final Iterator<IWorkbenchPart> itr = parts.iterator();
        while (itr.hasNext()) {
            final IWorkbenchPart part = itr.next();

            // get the part's ISetSelectionTarget implementation
            ISetSelectionTarget target = null;
            if (part instanceof ISetSelectionTarget) {
                target = (ISetSelectionTarget) part;
            } else {
                target = (ISetSelectionTarget) part.getAdapter(ISetSelectionTarget.class);
            }

            if (target != null) {
                // select and reveal resource
                final ISetSelectionTarget finalTarget = target;
                window.getShell().getDisplay().asyncExec(new Runnable() {
                    @Override
                    public void run() {
                        finalTarget.selectReveal(selection);
                    }
                });
            }
        }
    }

    public IWorkbench getWorkbench() {
        return workbench;
    }

    protected void updatePerspective() {
        ErlideUIPlugin.getDefault().showErlangPerspective();
    }

}
