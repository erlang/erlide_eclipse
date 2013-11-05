package org.erlide.core.internal.builder;

import com.google.common.base.Objects;
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
import org.erlide.engine.model.builder.BuilderConfig;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.builder.ErlangBuilder;
import org.erlide.engine.model.builder.IErlangBuilderFactory;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.ProjectConfigurationPersister;

/**
 * Erlang project nature
 * @author Eric Merritt [cyberlync at yahoo dot com]
 * @author Vlad Dumitrescu [vladdu55 att gmail dot com]
 */
@SuppressWarnings("all")
public class ErlangNature implements IProjectNature {
  private IProject project;
  
  public void configure() throws CoreException {
    ErlangNature.setErlangProjectBuilder(this.project, "internal");
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
  
  public static void setErlangProjectBuilder(final IProject prj, final String builderName) throws CoreException {
    ErlangNature.unsetAllErlangBuilders(prj);
    final IProjectDescription description = prj.getDescription();
    final ICommand[] old = description.getBuildSpec();
    int _length = old.length;
    int _plus = (_length + 1);
    final ICommand[] specs = new ICommand[_plus];
    int _length_1 = old.length;
    System.arraycopy(old, 0, specs, 0, _length_1);
    final ICommand command = description.newCommand();
    IErlangBuilderFactory _factory = ErlangBuilder.getFactory();
    String _upperCase = builderName.toUpperCase();
    ErlangBuilder _builder = _factory.getBuilder(_upperCase);
    String _id = _builder.getId();
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
    final Function1<BuilderTool,String> _function = new Function1<BuilderTool,String>() {
      public String apply(final BuilderTool it) {
        IErlangBuilderFactory _factory = ErlangBuilder.getFactory();
        String _name = it.name();
        ErlangBuilder _builder = _factory.getBuilder(_name);
        String _id = _builder.getId();
        return _id;
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
      _xblockexpression = (BuilderTool.INTERNAL);
    }
    return _xblockexpression;
  }
  
  public static BuilderConfig detectBuilderConfig(final IContainer folder) {
    BuilderConfig _xblockexpression = null;
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
        return BuilderConfig.EMAKE;
      }
      String _toolMarker_1 = BuilderTool.REBAR.getToolMarker();
      IResource _findMember_1 = folder.findMember(_toolMarker_1);
      boolean _tripleNotEquals_1 = (_findMember_1 != null);
      if (_tripleNotEquals_1) {
        return BuilderConfig.REBAR;
      }
      _xblockexpression = (BuilderConfig.INTERNAL);
    }
    return _xblockexpression;
  }
  
  /**
   * Returns the detected configuration for the project. Returns null if
   * impossible (project doesn't exist or files not available).
   */
  public static ErlangProjectProperties getConfig(final IErlProject project) {
    ErlangProjectProperties _xblockexpression = null;
    {
      IProject _workspaceProject = project.getWorkspaceProject();
      final BuilderConfig builderConfig = ErlangNature.detectBuilderConfig(_workspaceProject);
      IErlangBuilderFactory _factory = ErlangBuilder.getFactory();
      final ProjectConfigurationPersister persister = _factory.getConfigurationPersister(builderConfig);
      boolean _equals = Objects.equal(persister, null);
      if (_equals) {
        return null;
      }
      ErlangProjectProperties _configuration = persister.getConfiguration(project);
      _xblockexpression = (_configuration);
    }
    return _xblockexpression;
  }
}
