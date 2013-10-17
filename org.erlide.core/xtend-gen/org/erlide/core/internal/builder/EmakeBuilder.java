package org.erlide.core.internal.builder;

import java.io.StringBufferInputStream;
import java.util.Collection;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.InputOutput;
import org.erlide.backend.BackendCore;
import org.erlide.backend.api.IBackend;
import org.erlide.backend.api.IBackendManager;
import org.erlide.core.ErlangCore;
import org.erlide.core.builder.MarkerUtils;
import org.erlide.core.internal.builder.ExternalBuilder;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.IErlangEngine;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.root.BuilderConfigParser;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.IErlangProjectProperties;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.util.ErlLogger;

@SuppressWarnings("all")
public class EmakeBuilder extends ExternalBuilder {
  public String getOsCommand() {
    String _xblockexpression = null;
    {
      IBackendManager _backendManager = BackendCore.getBackendManager();
      IProject _project = this.getProject();
      final IBackend backend = _backendManager.getBuildBackend(_project);
      RuntimeInfo _runtimeInfo = backend.getRuntimeInfo();
      String _otpHome = _runtimeInfo.getOtpHome();
      Path _path = new Path(_otpHome);
      final IPath path = _path.append("bin/erl");
      String _oSString = path.toOSString();
      _xblockexpression = (_oSString);
    }
    return _xblockexpression;
  }
  
  protected String getCompileTarget() {
    return "-make";
  }
  
  protected String getCleanTarget() {
    return null;
  }
  
  public BuilderConfigParser getConfigParser() {
    return null;
  }
  
  public void clean(final IProgressMonitor monitor) {
    final IProject project = this.getProject();
    MarkerUtils.removeProblemMarkersFor(project);
    IErlangEngine _instance = ErlangEngine.getInstance();
    IErlModel _model = _instance.getModel();
    final IErlProject erlProject = _model.getErlangProject(project);
    IPath _outputLocation = erlProject.getOutputLocation();
    final IFolder bf = project.getFolder(_outputLocation);
    boolean _exists = bf.exists();
    if (_exists) {
      try {
        IResource[] _members = bf.members();
        for (final IResource f : _members) {
          try {
            f.delete(true, null);
          } catch (final Throwable _t) {
            if (_t instanceof CoreException) {
              final CoreException e = (CoreException)_t;
              IPath _location = bf.getLocation();
              String _plus = ("Could not clean up output directory " + _location);
              ErlLogger.warn(_plus);
            } else {
              throw Exceptions.sneakyThrow(_t);
            }
          }
        }
      } catch (final Throwable _t_1) {
        if (_t_1 instanceof CoreException) {
          final CoreException e_1 = (CoreException)_t_1;
        } else {
          throw Exceptions.sneakyThrow(_t_1);
        }
      }
    }
  }
  
  public void createConfig(final IErlangProjectProperties info) {
    IProject _project = this.getProject();
    final IFile config = _project.getFile("Emakefile");
    try {
      StringConcatenation _builder = new StringConcatenation();
      {
        Collection<IPath> _sourceDirs = info.getSourceDirs();
        for(final IPath src : _sourceDirs) {
          _builder.append("{\'");
          String _portableString = src.toPortableString();
          _builder.append(_portableString, "");
          _builder.append("/*\',[");
          {
            Collection<IPath> _includeDirs = info.getIncludeDirs();
            for(final IPath inc : _includeDirs) {
              _builder.append("{i, \"");
              String _portableString_1 = inc.toPortableString();
              _builder.append(_portableString_1, "");
              _builder.append("\"},");
            }
          }
          _builder.append("]}.");
          _builder.newLineIfNotEmpty();
        }
      }
      final String template = _builder.toString();
      String _plus = (">> " + template);
      InputOutput.<String>println(_plus);
      StringBufferInputStream _stringBufferInputStream = new StringBufferInputStream(template);
      config.create(_stringBufferInputStream, true, null);
    } catch (final Throwable _t) {
      if (_t instanceof CoreException) {
        final CoreException e = (CoreException)_t;
        ErlLogger.error(e);
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
  }
  
  public String getId() {
    String _plus = (ErlangCore.PLUGIN_ID + ".emake.builder");
    return _plus;
  }
}
