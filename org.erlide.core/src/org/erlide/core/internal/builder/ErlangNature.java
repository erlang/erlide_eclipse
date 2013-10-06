/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *******************************************************************************/
package org.erlide.core.internal.builder;

import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.erlide.core.ErlangCore;

/**
 * Erlang project nature
 * 
 * @author Eric Merritt [cyberlync at yahoo dot com]
 * @author Vlad Dumitrescu [vladdu55 att gmail dot com]
 */
public class ErlangNature implements IProjectNature {

    private IProject project;

    @Override
    public void configure() throws CoreException {
        final IProjectDescription description = project.getDescription();
        if (!hasBuildSpec(description.getBuildSpec())) {
            final ICommand[] old = description.getBuildSpec(), specs = new ICommand[old.length + 1];
            System.arraycopy(old, 0, specs, 0, old.length);
            final ICommand command = description.newCommand();
            command.setBuilderName(ErlangCore.BUILDER_ID);
            specs[old.length] = command;
            description.setBuildSpec(specs);
            project.setDescription(description, new NullProgressMonitor());
        }
    }

    @Override
    public void deconfigure() throws CoreException {
        final IProjectDescription description = project.getDescription();
        final int count = getBuildSpecCount(description.getBuildSpec());
        if (count != 0) {
            final ICommand[] old = description.getBuildSpec();
            final ICommand[] specs = new ICommand[old.length - count];
            int i = 0;
            int j = 0;
            while (j < old.length) {
                final String oldBuilderName = old[j].getBuilderName();
                if (!ErlangCore.BUILDER_ID.equals(oldBuilderName)) {
                    specs[i++] = old[j];
                }
                j++;
            }
            description.setBuildSpec(specs);
            project.setDescription(description, new NullProgressMonitor());
        }
    }

    @Override
    public IProject getProject() {
        return project;
    }

    @Override
    public void setProject(final IProject lproject) {
        project = lproject;
    }

    private boolean hasBuildSpec(final ICommand[] commands) {
        return getBuildSpecCount(commands) != 0;
    }

    private int getBuildSpecCount(final ICommand[] commands) {
        int count = 0;
        for (final ICommand element : commands) {
            if (ErlangCore.BUILDER_ID.equals(element.getBuilderName())) {
                count++;
            }
        }
        return count;
    }
}
