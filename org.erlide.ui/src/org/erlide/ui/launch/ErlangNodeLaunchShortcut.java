/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.launch;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.ui.ILaunchShortcut;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.erlide.backend.api.ErlRuntimeAttributes;
import org.erlide.backend.launch.IErlangLaunchDelegateConstants;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.ui.editors.erl.AbstractErlangEditor;
import org.erlide.util.ErlLogger;
import org.erlide.util.HostnameUtils;
import org.erlide.util.ListsUtils;
import org.erlide.util.StringUtils;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

public class ErlangNodeLaunchShortcut implements ILaunchShortcut {

    private static final String CONSOLE_VIEW_ID = "org.eclipse.ui.console.ConsoleView";
    public static final String PROJECT_NAME_SEPARATOR = ";";

    @Override
    public void launch(final ISelection selection, final String mode) {
        ErlLogger.debug("** Launch:: " + selection.toString());
        if (selection.isEmpty()) {
            return;
        }
        if (!(selection instanceof IStructuredSelection)) {
            return;
        }
        final Set<IErlProject> projects = Sets.newHashSet();
        final IStructuredSelection structuredSelection = (IStructuredSelection) selection;
        for (final Object element : structuredSelection.toArray()) {
            if (!(element instanceof IResource)) {
                return;
            }
            final IErlElement erlElement = ErlangEngine.getInstance().getModel()
                    .findElement((IResource) element);
            final IErlProject project = ErlangEngine.getInstance().getModelUtilService()
                    .getProject(erlElement);
            if (project != null) {
                projects.add(project);
            }
        }
        if (projects.isEmpty()) {
            return;
        }
        projects.addAll(getDependentProjects(projects));
        final List<IErlProject> projectList = Lists.newArrayList(projects);
        Collections.sort(projectList, new Comparator<IErlProject>() {
            @Override
            public int compare(final IErlProject o1, final IErlProject o2) {
                return o1.getName().compareTo(o2.getName());
            }
        });
        try {
            doLaunch(mode, projectList);
        } catch (final CoreException e) {
            final IWorkbench workbench = PlatformUI.getWorkbench();
            final Shell shell = workbench.getActiveWorkbenchWindow().getShell();
            MessageDialog.openError(shell, "Error", e.getStatus().getMessage());
        }
    }

    private Collection<IErlProject> getDependentProjects(final Set<IErlProject> projects) {
        final Set<IErlProject> depProjects = Sets.newHashSet();
        for (final IErlProject project : projects) {
            try {
                depProjects.addAll(project.getReferencedProjects());
            } catch (final ErlModelException e) {
            }
        }
        return depProjects;
    }

    @Override
    public void launch(final IEditorPart editor, final String mode) {
        ErlLogger.debug("** Launch :: " + editor.getTitle());
        if (editor instanceof AbstractErlangEditor) {
            final AbstractErlangEditor erlangEditor = (AbstractErlangEditor) editor;
            final IErlProject project = erlangEditor.getProject();
            if (project != null) {
                try {
                    doLaunch(mode, Lists.newArrayList(project));
                } catch (final CoreException e) {
                    final IWorkbench workbench = PlatformUI.getWorkbench();
                    final Shell shell = workbench.getActiveWorkbenchWindow().getShell();
                    MessageDialog.openError(shell, "Error", e.getStatus().getMessage());
                }
            }
        }
    }

    private void doLaunch(final String mode, final Collection<IErlProject> projects)
            throws CoreException {
        final ILaunchConfiguration launchConfiguration = getLaunchConfiguration(projects,
                mode);
        bringConsoleViewToFront();
        launchConfiguration.launch(mode, null);
    }

    private void bringConsoleViewToFront() throws PartInitException {
        final IWorkbenchPage activePage = PlatformUI.getWorkbench()
                .getActiveWorkbenchWindow().getActivePage();
        activePage.showView(CONSOLE_VIEW_ID);
    }

    private ILaunchConfiguration getLaunchConfiguration(
            final Collection<IErlProject> projects, final String mode)
            throws CoreException {
        final ILaunchManager launchManager = DebugPlugin.getDefault().getLaunchManager();
        final List<String> projectNames = getProjectNames(projects);
        String name = ListsUtils.packList(projectNames, "_");
        if (name.length() > 15) {
            name = ListsUtils.packList(StringUtils.removeCommonPrefixes(projectNames),
                    "_");
        }
        // try and find one
        final ILaunchConfiguration[] launchConfigurations = launchManager
                .getLaunchConfigurations();
        for (final ILaunchConfiguration launchConfiguration : launchConfigurations) {
            if (launchConfiguration.getName().equals(name)) {
                if (mode.equals(ILaunchManager.DEBUG_MODE)) {
                    return addInterpretedModules(projects, launchConfiguration);
                }
                return launchConfiguration;
            }
        }
        // try and make one
        final ILaunchConfigurationType launchConfigurationType = launchManager
                .getLaunchConfigurationType(IErlangLaunchDelegateConstants.CONFIGURATION_TYPE);
        ILaunchConfigurationWorkingCopy wc = null;
        wc = launchConfigurationType.newInstance(null, name);
        wc.setAttribute(ErlRuntimeAttributes.PROJECTS,
                ListsUtils.packList(projectNames, PROJECT_NAME_SEPARATOR));
        wc.setAttribute(ErlRuntimeAttributes.RUNTIME_NAME, projects.iterator().next()
                .getRuntimeInfo().getName());
        wc.setAttribute(ErlRuntimeAttributes.NODE_NAME, name);
        wc.setAttribute(ErlRuntimeAttributes.USE_LONG_NAME,
                !HostnameUtils.canUseShortNames()); // prefer short names
        wc.setAttribute(ErlRuntimeAttributes.CONSOLE, true);
        wc.setAttribute(ErlRuntimeAttributes.INTERNAL, false);
        wc.setAttribute(ErlRuntimeAttributes.LOAD_ALL_NODES, false);
        wc.setAttribute(ErlRuntimeAttributes.COOKIE, "erlide");
        final Map<String, String> map = Maps.newHashMap();
        wc.setAttribute("org.eclipse.debug.core.environmentVariables", map);
        if ("debug".equals(mode)) {
            final List<String> moduleNames = getProjectAndModuleNames(projects);
            wc.setAttribute(ErlRuntimeAttributes.DEBUG_INTERPRET_MODULES, moduleNames);
        }
        wc.setMappedResources(getProjectResources(projects));
        return wc.doSave();
    }

    private IResource[] getProjectResources(final Collection<IErlProject> projects) {
        final List<IResource> result = Lists.newArrayListWithCapacity(projects.size());
        for (final IErlProject project : projects) {
            result.add(project.getResource());
        }
        return result.toArray(new IResource[result.size()]);
    }

    private List<String> getProjectNames(final Collection<IErlProject> projects) {
        final List<String> result = Lists.newArrayListWithCapacity(projects.size());
        for (final IErlProject project : projects) {
            String name = project.getName();
            if (name.startsWith("org.erlide.")) {
                name = name.substring("org.erlide.".length());
            }
            result.add(name.replaceAll("\\.", ""));
        }
        return result;
    }

    private List<String> getProjectAndModuleNames(final Collection<IErlProject> projects)
            throws ErlModelException {
        final List<String> moduleNames = Lists.newArrayList();
        for (final IErlProject project : projects) {
            final Collection<IErlModule> modules = project.getModules();
            final String projectNameColon = project.getName() + ":";
            for (final IErlModule module : modules) {
                moduleNames.add(projectNameColon + module.getName());
            }
        }
        return moduleNames;
    }

    private ILaunchConfiguration addInterpretedModules(
            final Collection<IErlProject> projects,
            final ILaunchConfiguration launchConfiguration) throws CoreException {
        final List<String> moduleNames = getProjectAndModuleNames(projects);
        final ILaunchConfigurationWorkingCopy wc = launchConfiguration.getWorkingCopy();
        wc.setAttribute(ErlRuntimeAttributes.DEBUG_INTERPRET_MODULES, moduleNames);
        return wc.doSave();
    }

}
