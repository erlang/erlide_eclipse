package org.erlide.core.internal.builder.external;

import java.util.Map;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.ErlangCore;
import org.erlide.core.builder.MarkerUtils;
import org.erlide.core.internal.builder.ExternalBuilder;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.IErlangEngine;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.builder.BuildProperties;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlFolder;

@SuppressWarnings("all")
public class RebarBuilder extends ExternalBuilder {
  public String getOsCommand() {
    return "rebar";
  }
  
  public IProject[] build(final int kind, final Map<String,String> args, final IProgressMonitor monitor) throws CoreException {
    IProject[] _xblockexpression = null;
    {
      final IProject[] result = super.build(kind, args, monitor);
      this.checkIfProjectHasAppFile();
      _xblockexpression = (result);
    }
    return _xblockexpression;
  }
  
  private boolean foundAppSrc;
  
  private IMarker checkIfProjectHasAppFile() throws CoreException {
    IMarker _xblockexpression = null;
    {
      this.foundAppSrc = false;
      IProject _project = this.getProject();
      final IResourceVisitor _function = new IResourceVisitor() {
        public boolean visit(final IResource resource) throws CoreException {
          boolean _xblockexpression = false;
          {
            String _name = resource.getName();
            boolean _endsWith = _name.endsWith(".app.src");
            if (_endsWith) {
              IErlangEngine _instance = ErlangEngine.getInstance();
              IErlModel _model = _instance.getModel();
              IContainer _parent = resource.getParent();
              IErlElement _findElement = _model.findElement(_parent);
              final IErlFolder folder = ((IErlFolder) _findElement);
              boolean _and = false;
              boolean _tripleNotEquals = (folder != null);
              if (!_tripleNotEquals) {
                _and = false;
              } else {
                boolean _isOnSourcePath = folder.isOnSourcePath();
                _and = (_tripleNotEquals && _isOnSourcePath);
              }
              if (_and) {
                RebarBuilder.this.foundAppSrc = true;
              }
            }
            boolean _not = (!RebarBuilder.this.foundAppSrc);
            _xblockexpression = (_not);
          }
          return _xblockexpression;
        }
      };
      _project.accept(_function);
      IMarker _xifexpression = null;
      boolean _not = (!this.foundAppSrc);
      if (_not) {
        IProject _project_1 = this.getProject();
        int _minus = (-1);
        IMarker _addMarker = MarkerUtils.addMarker(null, _project_1, null, "No .app.src file found, can\'t compile with rebar", _minus, 
          IMarker.SEVERITY_ERROR, IMarker.PROBLEM);
        _xifexpression = _addMarker;
      }
      _xblockexpression = (_xifexpression);
    }
    return _xblockexpression;
  }
  
  public String getId() {
    String _plus = (ErlangCore.PLUGIN_ID + ".rebar.builder");
    return _plus;
  }
  
  public BuildProperties getProperties() {
    return null;
  }
}
