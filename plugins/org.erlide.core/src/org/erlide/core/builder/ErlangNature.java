/**
 * Copyright (c) 2004 Eric Merritt and others. All rights reserved. This program and the
 * accompanying materials are made available under the terms of the Eclipse Public License
 * v1.0 which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Eric Merritt Vlad Dumitrescu
 */
package org.erlide.core.builder;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.ICommand;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IProjectNature;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.ListExtensions;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.ProjectConfigType;

@SuppressWarnings("all")
public class ErlangNature implements IProjectNature {
    private IProject project;

    @Override
    public void configure() throws CoreException {
        ErlangNature.setErlangProjectBuilder(project, BuilderTool.INTERNAL);
    }

    @Override
    public void deconfigure() throws CoreException {
        ErlangNature.unsetAllErlangBuilders(project);
    }

    @Override
    public IProject getProject() {
        return project;
    }

    @Override
    public void setProject(final IProject lproject) {
        project = lproject;
    }

    public static void setErlangProjectBuilder(final IProject prj,
            final BuilderTool builder) throws CoreException {
        ErlangNature.unsetAllErlangBuilders(prj);
        final IProjectDescription description = prj.getDescription();
        final ICommand[] old = description.getBuildSpec();
        final int _length = old.length;
        final int _plus = _length + 1;
        final ICommand[] specs = new ICommand[_plus];
        System.arraycopy(old, 0, specs, 0, old.length);
        final ICommand command = description.newCommand();
        command.setBuilderName(builder.getId());
        specs[old.length] = command;
        description.setBuildSpec(specs);
        final NullProgressMonitor _nullProgressMonitor = new NullProgressMonitor();
        prj.setDescription(description, _nullProgressMonitor);
    }

    public static void unsetAllErlangBuilders(final IProject prj) throws CoreException {
        final IProjectDescription description = prj.getDescription();
        final ICommand[] old = description.getBuildSpec();
        final Function1<BuilderTool, String> _function = (final BuilderTool it) -> it
                .getId();
        final List<String> allIds = ListExtensions.<BuilderTool, String> map(
                (List<BuilderTool>) Conversions.doWrapArray(BuilderTool.values()),
                _function);
        final ArrayList<ICommand> specs = CollectionLiterals.<ICommand> newArrayList();
        for (final ICommand cmd : old) {
            {
                final String oldBuilderName = cmd.getBuilderName();
                final boolean _contains = allIds.contains(oldBuilderName);
                final boolean _not = !_contains;
                if (_not) {
                    specs.add(cmd);
                }
            }
        }
        description.setBuildSpec(
                (ICommand[]) Conversions.unwrapArray(specs, ICommand.class));
        final NullProgressMonitor _nullProgressMonitor = new NullProgressMonitor();
        prj.setDescription(description, _nullProgressMonitor);
    }

    public static BuilderTool detectBuilderTool(final IContainer folder) {
        BuilderTool _xblockexpression = null;
        {
            final boolean _exists = folder.exists();
            final boolean _not = !_exists;
            if (_not) {
                return null;
            }
            final IResource _findMember = folder
                    .findMember(BuilderTool.MAKE.getToolMarker());
            final boolean _tripleNotEquals = _findMember != null;
            if (_tripleNotEquals) {
                return BuilderTool.MAKE;
            }
            final IResource _findMember_1 = folder
                    .findMember(BuilderTool.EMAKE.getToolMarker());
            final boolean _tripleNotEquals_1 = _findMember_1 != null;
            if (_tripleNotEquals_1) {
                return BuilderTool.EMAKE;
            }
            final IResource _findMember_2 = folder
                    .findMember(BuilderTool.REBAR.getToolMarker());
            final boolean _tripleNotEquals_2 = _findMember_2 != null;
            if (_tripleNotEquals_2) {
                return BuilderTool.REBAR;
            }
            _xblockexpression = BuilderTool.INTERNAL;
        }
        return _xblockexpression;
    }

    public static ProjectConfigType detectBuilderConfig(final IContainer folder) {
        ProjectConfigType _xblockexpression = null;
        {
            final boolean _exists = folder.exists();
            final boolean _not = !_exists;
            if (_not) {
                return null;
            }
            final IResource _findMember = folder
                    .findMember(BuilderTool.EMAKE.getToolMarker());
            final boolean _tripleNotEquals = _findMember != null;
            if (_tripleNotEquals) {
                return ProjectConfigType.EMAKE;
            }
            final IResource _findMember_1 = folder
                    .findMember(BuilderTool.REBAR.getToolMarker());
            final boolean _tripleNotEquals_1 = _findMember_1 != null;
            if (_tripleNotEquals_1) {
                return ProjectConfigType.REBAR;
            }
            _xblockexpression = ProjectConfigType.INTERNAL;
        }
        return _xblockexpression;
    }
}
