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

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.erlide.core.ErlangCore;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

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
        setProjectBuilder(project, "internal");
    }

    @Override
    public void deconfigure() throws CoreException {
        unsetAllBuilders(project);
    }

    @Override
    public IProject getProject() {
        return project;
    }

    @Override
    public void setProject(final IProject lproject) {
        project = lproject;
    }

    private static boolean hasBuildSpec(final ICommand[] commands) {
        int count = 0;
        for (final ICommand element : commands) {
            if (ErlangCore.BUILDER_ID.equals(element.getBuilderName())) {
                count++;
            }
        }
        final int buildSpecCount = count;
        return buildSpecCount != 0;
    }

    private final static Collection<String> ALL_BUILDER_IDS = Lists
            .newArrayList(ErlangCore.BUILDER_ID, ErlangCore.MAKEBUILDER_ID,
                    ErlangCore.EMAKEBUILDER_ID, ErlangCore.REBARBUILDER_ID);

    private final static Map<String, String> BUILDER_ID_MAP = Maps.newHashMap();
    static {
        BUILDER_ID_MAP.put("internal", ErlangCore.BUILDER_ID);
        BUILDER_ID_MAP.put("make", ErlangCore.MAKEBUILDER_ID);
        BUILDER_ID_MAP.put("emake", ErlangCore.EMAKEBUILDER_ID);
        BUILDER_ID_MAP.put("rebar", ErlangCore.REBARBUILDER_ID);
    }

    public static void setProjectBuilder(final IProject prj,
            final String builderName) throws CoreException {
        unsetAllBuilders(prj);

        final IProjectDescription description = prj.getDescription();
        final ICommand[] old = description.getBuildSpec();
        final ICommand[] specs = new ICommand[old.length + 1];
        System.arraycopy(old, 0, specs, 0, old.length);
        final ICommand command = description.newCommand();
        command.setBuilderName(BUILDER_ID_MAP.get(builderName));
        specs[old.length] = command;

        description.setBuildSpec(specs);
        prj.setDescription(description, new NullProgressMonitor());
    }

    public static void unsetAllBuilders(final IProject prj)
            throws CoreException {
        final IProjectDescription description = prj.getDescription();
        final ICommand[] old = description.getBuildSpec();
        final List<ICommand> specs = Lists.newArrayList();
        for (final ICommand cmd : old) {
            final String oldBuilderName = cmd.getBuilderName();
            if (!ALL_BUILDER_IDS.contains(oldBuilderName)) {
                specs.add(cmd);
            }
        }
        description.setBuildSpec(specs.toArray(new ICommand[specs.size()]));
        prj.setDescription(description, new NullProgressMonitor());
    }

}
