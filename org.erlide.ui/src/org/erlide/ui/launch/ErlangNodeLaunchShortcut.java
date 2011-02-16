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

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.ui.ILaunchShortcut;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IEditorPart;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlProject;
import org.erlide.jinterface.util.ErlLogger;

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
        final ILaunchConfiguration launchConfiguration = getLaunchConfiguration(project);
        try {
            launchConfiguration.launch(mode, null);
        } catch (final CoreException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    private ILaunchConfiguration getLaunchConfiguration(
            final IErlProject project) {
        final ILaunchManager launchManager = DebugPlugin.getDefault()
                .getLaunchManager();
        try {
            final ILaunchConfiguration[] launchConfigurations = launchManager
                    .getLaunchConfigurations();
            for (final ILaunchConfiguration launchConfiguration : launchConfigurations) {
                if (launchConfiguration.getName().equals(project.getName())) {
                    return launchConfiguration;
                }
            }
        } catch (final CoreException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return null;
    }

    public void launch(final IEditorPart editor, final String mode) {
        // TODO Auto-generated method stub
        ErlLogger.debug("** Launch :: " + editor.getTitle());
    }

}
