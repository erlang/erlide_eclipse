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
import org.erlide.core.internal.builder.BuildersInfo;
import org.erlide.core.internal.builder.ErlangBuilder;

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
    String _upperCase = builderName.toUpperCase();
    BuildersInfo _valueOf = BuildersInfo.valueOf(_upperCase);
    ErlangBuilder _impl = _valueOf.getImpl();
    String _id = _impl.getId();
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
    BuildersInfo[] _values = BuildersInfo.values();
    final Function1<BuildersInfo,String> _function = new Function1<BuildersInfo,String>() {
      public String apply(final BuildersInfo it) {
        ErlangBuilder _impl = it.getImpl();
        String _id = _impl.getId();
        return _id;
      }
    };
    final List<String> allIds = ListExtensions.<BuildersInfo, String>map(((List<BuildersInfo>)Conversions.doWrapArray(_values)), _function);
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
  
  public static String detectBuilder(final IContainer folder) {
    String _xblockexpression = null;
    {
      IResource _findMember = folder.findMember("Emakefile");
      boolean _tripleNotEquals = (_findMember != null);
      if (_tripleNotEquals) {
        return "emake";
      }
      IResource _findMember_1 = folder.findMember("Makefile");
      boolean _tripleNotEquals_1 = (_findMember_1 != null);
      if (_tripleNotEquals_1) {
        return "make";
      }
      IResource _findMember_2 = folder.findMember("rebar.config");
      boolean _tripleNotEquals_2 = (_findMember_2 != null);
      if (_tripleNotEquals_2) {
        return "rebar";
      }
      _xblockexpression = ("internal");
    }
    return _xblockexpression;
  }
}
