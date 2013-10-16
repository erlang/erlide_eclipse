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

import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.erlide.util.ErlLogger;

import com.google.common.base.Function;
import com.google.common.collect.Iterables;
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
        setErlangProjectBuilder(project, "internal");
    }

    @Override
    public void deconfigure() throws CoreException {
        unsetAllErlangBuilders(project);
    }

    @Override
    public IProject getProject() {
        return project;
    }

    @Override
    public void setProject(final IProject lproject) {
        project = lproject;
    }

    public final static Map<String, Class<? extends ErlangBuilder>> BUILDER_MAP = Maps
            .newHashMap();
    static {
        BUILDER_MAP.put("internal", InternalBuilder.class);
        BUILDER_MAP.put("make", MakeBuilder.class);
        BUILDER_MAP.put("emake", EmakeBuilder.class);
        BUILDER_MAP.put("rebar", RebarBuilder.class);
    }

    public static void setErlangProjectBuilder(final IProject prj,
            final String builderName) throws CoreException {
        unsetAllErlangBuilders(prj);

        final IProjectDescription description = prj.getDescription();
        final ICommand[] old = description.getBuildSpec();
        final ICommand[] specs = new ICommand[old.length + 1];
        System.arraycopy(old, 0, specs, 0, old.length);
        final ICommand command = description.newCommand();
        try {
            command.setBuilderName(BUILDER_MAP.get(builderName).newInstance().getId());
            specs[old.length] = command;
            description.setBuildSpec(specs);
            prj.setDescription(description, new NullProgressMonitor());
        } catch (final InstantiationException e) {
            ErlLogger.error(e);
        } catch (final IllegalAccessException e) {
            ErlLogger.error(e);
        }
    }

    public static void unsetAllErlangBuilders(final IProject prj) throws CoreException {
        final IProjectDescription description = prj.getDescription();
        final ICommand[] old = description.getBuildSpec();
        final List<String> allIds = Lists.newArrayList(Iterables.transform(
                BUILDER_MAP.values(),
                new Function<Class<? extends ErlangBuilder>, String>() {
                    @Override
                    public final String apply(final Class<? extends ErlangBuilder> input) {
                        try {
                            return input.newInstance().getId();
                        } catch (final InstantiationException e) {
                            ErlLogger.error(e);
                        } catch (final IllegalAccessException e) {
                            ErlLogger.error(e);
                        }
                        return null;
                    }
                }));
        final List<ICommand> specs = Lists.newArrayList();
        for (final ICommand cmd : old) {
            final String oldBuilderName = cmd.getBuilderName();
            if (!allIds.contains(oldBuilderName)) {
                specs.add(cmd);
            }
        }
        description.setBuildSpec(specs.toArray(new ICommand[specs.size()]));
        prj.setDescription(description, new NullProgressMonitor());
    }

    public static ErlangBuilder getBuilder(final String builderName) {
        if ("internal".equals(builderName)) {
            return new InternalBuilder();
        }
        if ("rebar".equals(builderName)) {
            return new RebarBuilder();
        }
        if ("emake".equals(builderName)) {
            return new EmakeBuilder();
        }
        if ("make".equals(builderName)) {
            return new MakeBuilder();
        }
        throw new IllegalArgumentException("unknown builder: " + builderName);
    }

}
