package org.erlide.core.internal.builder.external;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.backend.BackendCore;
import org.erlide.backend.api.IBackend;
import org.erlide.backend.api.IBackendManager;
import org.erlide.core.internal.builder.BuildNotifier;
import org.erlide.core.internal.builder.ExternalBuilder;
import org.erlide.engine.model.builder.BuilderProperties;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.builder.MarkerUtils;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.api.IOtpNodeProxy;
import org.erlide.util.ErlLogger;
import org.erlide.util.SystemConfiguration;

@SuppressWarnings("all")
public class EmakeBuilder extends ExternalBuilder {
  public EmakeBuilder() {
    super(BuilderTool.EMAKE);
  }
  
  public String getOsCommand(final IErlProject erlProject) {
    String _xblockexpression = null;
    {
      IBackendManager _backendManager = BackendCore.getBackendManager();
      final IBackend backend = _backendManager.getBuildBackend(erlProject);
      IOtpNodeProxy _runtime = backend.getRuntime();
      String _otpHome = _runtime.getOtpHome();
      Path _path = new Path(_otpHome);
      final IPath path = _path.append("bin/erl");
      String _xifexpression = null;
      SystemConfiguration _instance = SystemConfiguration.getInstance();
      boolean _isOnWindows = _instance.isOnWindows();
      if (_isOnWindows) {
        String _portableString = path.toPortableString();
        _xifexpression = (_portableString + ".exe");
      } else {
        _xifexpression = path.toPortableString();
      }
      _xblockexpression = _xifexpression;
    }
    return _xblockexpression;
  }
  
  protected String getCompileTarget() {
    return "-make";
  }
  
  protected String getCleanTarget() {
    return null;
  }
  
  public void clean(final IErlProject erlProject, final BuildNotifier notifier) {
    final IProject project = erlProject.getWorkspaceProject();
    MarkerUtils.removeProblemMarkersFor(project);
    ErlangProjectProperties _properties = erlProject.getProperties();
    IPath _outputDir = _properties.getOutputDir();
    final IFolder bf = project.getFolder(_outputDir);
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
  
  public BuilderProperties getProperties() {
    return null;
  }
}
