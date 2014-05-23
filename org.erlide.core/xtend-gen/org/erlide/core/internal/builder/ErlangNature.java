/**
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Eric Merritt
 *     Vlad Dumitrescu
 */
package org.erlide.core.internal.builder;

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
  
  public void configure() throws CoreException {
    ErlangNature.setErlangProjectBuilder(this.project, BuilderTool.INTERNAL);
  }
  
  public void deconfigure() throws CoreException {
    ErlangNature.unsetAllErlangBuilders(this.project);
  }
  
  public IProject getProject() {
    return this.project;
  }
  
  public void setProject(final IProject lproject) {
    this.project = lproject;
  }
  
  public static void setErlangProjectBuilder(final IProject prj, final BuilderTool builder) throws CoreException {
    ErlangNature.unsetAllErlangBuilders(prj);
    final IProjectDescription description = prj.getDescription();
    final ICommand[] old = description.getBuildSpec();
    int _length = old.length;
    int _plus = (_length + 1);
    final ICommand[] specs = new ICommand[_plus];
    int _length_1 = old.length;
    System.arraycopy(old, 0, specs, 0, _length_1);
    final ICommand command = description.newCommand();
    String _id = builder.getId();
    command.setBuilderName(_id);
    int _length_2 = old.length;
    specs[_length_2] = command;
    description.setBuildSpec(specs);
    NullProgressMonitor _nullProgressMonitor = new NullProgressMonitor();
    prj.setDescription(description, _nullProgressMonitor);
  }
  
  public static void unsetAllErlangBuilders(final IProject prj) throws CoreException {
    final IProjectDescription description = prj.getDescription();
    final ICommand[] old = description.getBuildSpec();
    BuilderTool[] _values = BuilderTool.values();
    final Function1<BuilderTool, String> _function = new Function1<BuilderTool, String>() {
      public String apply(final BuilderTool it) {
        return it.getId();
      }
    };
    final List<String> allIds = ListExtensions.<BuilderTool, String>map(((List<BuilderTool>)Conversions.doWrapArray(_values)), _function);
    final ArrayList<ICommand> specs = CollectionLiterals.<ICommand>newArrayList();
    for (final ICommand cmd : old) {
      {
        final String oldBuilderName = cmd.getBuilderName();
        boolean _contains = allIds.contains(oldBuilderName);
        boolean _not = (!_contains);
        if (_not) {
          specs.add(cmd);
        }
      }
    }
    description.setBuildSpec(((ICommand[])Conversions.unwrapArray(specs, ICommand.class)));
    NullProgressMonitor _nullProgressMonitor = new NullProgressMonitor();
    prj.setDescription(description, _nullProgressMonitor);
  }
  
  public static BuilderTool detectBuilderTool(final IContainer folder) {
    BuilderTool _xblockexpression = null;
    {
      boolean _exists = folder.exists();
      boolean _not = (!_exists);
      if (_not) {
        return null;
      }
      String _toolMarker = BuilderTool.MAKE.getToolMarker();
      IResource _findMember = folder.findMember(_toolMarker);
      boolean _tripleNotEquals = (_findMember != null);
      if (_tripleNotEquals) {
        return BuilderTool.MAKE;
      }
      String _toolMarker_1 = BuilderTool.EMAKE.getToolMarker();
      IResource _findMember_1 = folder.findMember(_toolMarker_1);
      boolean _tripleNotEquals_1 = (_findMember_1 != null);
      if (_tripleNotEquals_1) {
        return BuilderTool.EMAKE;
      }
      String _toolMarker_2 = BuilderTool.REBAR.getToolMarker();
      IResource _findMember_2 = folder.findMember(_toolMarker_2);
      boolean _tripleNotEquals_2 = (_findMember_2 != null);
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
      boolean _exists = folder.exists();
      boolean _not = (!_exists);
      if (_not) {
        return null;
      }
      String _toolMarker = BuilderTool.EMAKE.getToolMarker();
      IResource _findMember = folder.findMember(_toolMarker);
      boolean _tripleNotEquals = (_findMember != null);
      if (_tripleNotEquals) {
        return ProjectConfigType.EMAKE;
      }
      String _toolMarker_1 = BuilderTool.REBAR.getToolMarker();
      IResource _findMember_1 = folder.findMember(_toolMarker_1);
      boolean _tripleNotEquals_1 = (_findMember_1 != null);
      if (_tripleNotEquals_1) {
        return ProjectConfigType.REBAR;
      }
      _xblockexpression = ProjectConfigType.INTERNAL;
    }
    return _xblockexpression;
  }
}
