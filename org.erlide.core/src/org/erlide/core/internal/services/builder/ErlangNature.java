/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Eric Merritt
 *******************************************************************************/
package org.erlide.core.internal.services.builder;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.erlide.core.ErlangCore;
import org.erlide.jinterface.ErlLogger;

/**
 * Simple project nature
 * 
 * 
 * @author Eric Merritt [cyberlync at yahoo dot com]
 */
public class ErlangNature implements IProjectNature {

    private IProject project;

    /**
     * Not used yet; this is like ErlangCore for a specific project.
     * 
     * @param project
     *            the project we want to know about (if it is null, null is
     *            returned)
     * @return the erlang nature for a project (or null if it does not exist for
     *         the project)
     * 
     * @note: it's synchronized because more than 1 place could call
     *        getErlangNature at the same time and more than one nature ended up
     *        being created from project.getNature().
     */
    public static synchronized ErlangNature getErlangNature(
            final IProject project) {
        if (project != null && project.isOpen()) {
            try {
                final IProjectNature n = project
                        .getNature(ErlangCore.NATURE_ID);
                if (n instanceof ErlangNature) {
                    return (ErlangNature) n;
                }
            } catch (final CoreException e) {
                ErlLogger.info(e);
            }
        }
        return null;
    }

    /**
     * @return all the Erlang natures available in the workspace (for opened and
     *         existing projects)
     */
    public static List<ErlangNature> getAllErlangNatures() {
        final List<ErlangNature> natures = new ArrayList<ErlangNature>();
        final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        final IProject[] projects = root.getProjects();
        for (final IProject project : projects) {
            final ErlangNature nature = getErlangNature(project);
            if (nature != null) {
                natures.add(nature);
            }
        }
        return natures;
    }

    /**
     * Configure the nature
     * 
     * @see org.eclipse.core.resources.IProjectNature#configure()
     */
    @Override
    public void configure() throws CoreException {
        final IProjectDescription description = project.getDescription();
        if (!hasBuildSpec(description.getBuildSpec())) {
            final ICommand[] old = description.getBuildSpec(), specs = new ICommand[old.length + 2];
            System.arraycopy(old, 0, specs, 0, old.length);
            ICommand command = description.newCommand();
            command.setBuilderName(ErlangCore.BUILDER_ID);
            specs[old.length] = command;
            command = description.newCommand();
            command.setBuilderName(DialyzerBuilder.BUILDER_ID);
            specs[old.length + 1] = command;
            description.setBuildSpec(specs);
            project.setDescription(description, new NullProgressMonitor());
        }
    }

    /**
     * deconfigure the nature
     * 
     * @see org.eclipse.core.resources.IProjectNature#deconfigure()
     */
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
                if (!ErlangCore.BUILDER_ID.equals(oldBuilderName)
                        && !DialyzerBuilder.BUILDER_ID.equals(oldBuilderName)) {
                    specs[i++] = old[j];
                }
                j++;
            }
            description.setBuildSpec(specs);
            project.setDescription(description, new NullProgressMonitor());
        }
    }

    /**
     * Get the project
     * 
     * @see org.eclipse.core.resources.IProjectNature#getProject()
     */
    @Override
    public IProject getProject() {
        return project;
    }

    /**
     * Set the project
     * 
     * @see org.eclipse.core.resources.IProjectNature#setProject(org.eclipse.core.resources.IProject)
     */
    @Override
    public void setProject(final IProject lproject) {
        project = lproject;
    }

    /**
     * Test for buildspec
     * 
     * @param commands
     * @return
     */
    private boolean hasBuildSpec(final ICommand[] commands) {
        return getBuildSpecCount(commands) != 0;
    }

    /**
     * Get spec count
     * 
     * @param commands
     * @return
     */
    private int getBuildSpecCount(final ICommand[] commands) {
        int count = 0;
        for (final ICommand element : commands) {
            if (ErlangCore.BUILDER_ID.equals(element.getBuilderName())
                    || DialyzerBuilder.BUILDER_ID.equals(element
                            .getBuilderName())) {
                count++;
            }
        }
        return count;
    }
}
