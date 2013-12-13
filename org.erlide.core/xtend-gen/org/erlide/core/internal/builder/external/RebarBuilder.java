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
import org.erlide.core.internal.builder.ExternalBuilder;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.IErlangEngine;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.builder.BuilderProperties;
import org.erlide.engine.model.builder.MarkerUtils;
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
            _xblockexpression = ((!RebarBuilder.this.foundAppSrc));
          }
          return _xblockexpression;
        }
      };
      _project.accept(_function);
      IMarker _xifexpression = null;
      if ((!this.foundAppSrc)) {
        IProject _project_1 = this.getProject();
        IProject _project_2 = this.getProject();
        IMarker _addMarker = MarkerUtils.addMarker(_project_1, _project_2, null, "No .app.src file found, can\'t compile with rebar", (-1), 
          IMarker.SEVERITY_WARNING, MarkerUtils.PROBLEM_MARKER);
        _xifexpression = _addMarker;
      }
      _xblockexpression = (_xifexpression);
    }
    return _xblockexpression;
  }
  
  public String getId() {
    return (ErlangCore.PLUGIN_ID + ".rebar.builder");
  }
  
  public BuilderProperties getProperties() {
    return null;
  }
}
