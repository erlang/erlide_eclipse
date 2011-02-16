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
import java.util.List;

import org.eclipse.core.resources.IProject;
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
import org.eclipse.ui.PlatformUI;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.jinterface.backend.ErlLaunchAttributes;
import org.erlide.jinterface.backend.ErtsProcess;
import org.erlide.jinterface.util.ErlLogger;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public class ErlangNodeLaunchShortcut implements ILaunchShortcut {

    public void launch(final ISelection selection, final String mode) {
        ErlLogger.debug("** Launch:: " + selection.toString());
        if (selection.isEmpty()) {
            return;
        }
        if (!(selection instanceof IStructuredSelection)) {
            return;
        }
        final IStructuredSelection structuredSelection = (IStructuredSelection) selection;
        final Object firstElement = structuredSelection.getFirstElement();
        if (!(firstElement instanceof IProject)) {
            return;
        }
        final IErlProject project = ErlangCore.getModel().getErlangProject(
                (IProject) firstElement);
        final ILaunchConfiguration launchConfiguration = getLaunchConfiguration(
                project, mode);
        try {
            launchConfiguration.launch(mode, null);
        } catch (final CoreException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    private ILaunchConfiguration getLaunchConfiguration(
            final IErlProject project, final String mode) {
        final ILaunchManager launchManager = DebugPlugin.getDefault()
                .getLaunchManager();
        final String name = project.getName();
        try {
            // try and find one
            final ILaunchConfiguration[] launchConfigurations = launchManager
                    .getLaunchConfigurations();
            for (final ILaunchConfiguration launchConfiguration : launchConfigurations) {
                if (launchConfiguration.getName().equals(name)) {
                    if (mode.equals("debug")) {
                        return addInterpretedModules(project,
                                launchConfiguration);
                    }
                    return launchConfiguration;
                }
            }
        } catch (final CoreException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        // try and make one
        final ILaunchConfigurationType launchConfigurationType = launchManager
                .getLaunchConfigurationType(ErtsProcess.CONFIGURATION_TYPE);
        ILaunchConfigurationWorkingCopy wc = null;
        try {
            wc = launchConfigurationType.newInstance(null, name);
            wc.setAttribute(ErlLaunchAttributes.PROJECTS, name);
            wc.setAttribute(ErlLaunchAttributes.RUNTIME_NAME, project
                    .getRuntimeInfo().getName());
            wc.setAttribute(ErlLaunchAttributes.NODE_NAME, name);
            wc.setAttribute(ErlLaunchAttributes.CONSOLE, true);
            wc.setAttribute(ErlLaunchAttributes.INTERNAL, false);
            wc.setAttribute(ErlLaunchAttributes.LOAD_ALL_NODES, false);
            wc.setAttribute("org.eclipse.debug.core.environmentVariables",
                    Maps.newHashMap());
            if (mode.equals("debug")) {
                final List<String> moduleNames = getProjectAndModuleNames(project);
                wc.setAttribute(ErlLaunchAttributes.DEBUG_INTERPRET_MODULES,
                        moduleNames);
            }
            wc.setMappedResources(new IResource[] { project.getResource() });
            return wc.doSave();
        } catch (final CoreException exception) {
            final IWorkbench workbench = PlatformUI.getWorkbench();
            final Shell shell = workbench.getActiveWorkbenchWindow().getShell();
            MessageDialog.openError(shell, "Error", exception.getStatus()
                    .getMessage());
        }
        return null;
    }

    private List<String> getProjectAndModuleNames(final IErlProject project) {
        final List<String> moduleNames = Lists.newArrayList();
        try {
            final Collection<IErlModule> modules = project.getModules();
            final String projectNameColon = project.getName() + ":";
            for (final IErlModule module : modules) {
                moduleNames.add(projectNameColon + module.getName());
            }
        } catch (final ErlModelException e) {
        }
        return moduleNames;
    }

    private ILaunchConfiguration addInterpretedModules(
            final IErlProject project,
            final ILaunchConfiguration launchConfiguration) {
        final List<String> moduleNames = getProjectAndModuleNames(project);
        try {
            final ILaunchConfigurationWorkingCopy wc = launchConfiguration
                    .getWorkingCopy();
            wc.setAttribute(ErlLaunchAttributes.DEBUG_INTERPRET_MODULES,
                    moduleNames);
            return wc.doSave();
        } catch (final CoreException e) {
            return launchConfiguration;
        }
    }

    public void launch(final IEditorPart editor, final String mode) {
        // TODO Auto-generated method stub
        ErlLogger.debug("** Launch :: " + editor.getTitle());
    }

}
