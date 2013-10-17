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
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.InputOutput;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
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
import org.erlide.util.SystemConfiguration;

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
      String _xifexpression = null;
      SystemConfiguration _instance = SystemConfiguration.getInstance();
      boolean _isOnWindows = _instance.isOnWindows();
      if (_isOnWindows) {
        String _portableString = path.toPortableString();
        String _plus = (_portableString + ".exe");
        _xifexpression = _plus;
      } else {
        String _portableString_1 = path.toPortableString();
        _xifexpression = _portableString_1;
      }
      _xblockexpression = (_xifexpression);
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
    final Procedure1<IFolder> _function = new Procedure1<IFolder>() {
      public void apply(final IFolder it) {
        try {
          boolean _exists = it.exists();
          if (_exists) {
            IResource[] _members = it.members();
            final Procedure1<IResource> _function = new Procedure1<IResource>() {
              public void apply(final IResource it) {
                try {
                  it.delete(true, null);
                } catch (final Throwable _t) {
                  if (_t instanceof CoreException) {
                    final CoreException e = (CoreException)_t;
                    IPath _location = it.getLocation();
                    String _plus = ("Could not clean up output directory " + _location);
                    ErlLogger.warn(_plus);
                  } else {
                    throw Exceptions.sneakyThrow(_t);
                  }
                }
              }
            };
            IterableExtensions.<IResource>forEach(((Iterable<IResource>)Conversions.doWrapArray(_members)), _function);
          }
        } catch (Throwable _e) {
          throw Exceptions.sneakyThrow(_e);
        }
      }
    };
    ObjectExtensions.<IFolder>operator_doubleArrow(bf, _function);
  }
  
  public void createConfig(final IProject project, final IErlangProjectProperties info) {
    final IFile config = project.getFile("Emakefile");
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
