package org.erlide.core.internal.builder.external;

import java.util.Map;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.internal.builder.ExternalBuilder;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.IErlangEngine;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.builder.BuilderProperties;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.builder.MarkerUtils;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlFolder;

@SuppressWarnings("all")
public class RebarBuilder extends ExternalBuilder {
  public RebarBuilder() {
    super(BuilderTool.REBAR);
  }
  
  public IProject[] build(final int kind, final Map<String, String> args, final IProgressMonitor monitor) throws CoreException {
    IProject[] _xblockexpression = null;
    {
      final IProject[] result = super.build(kind, args, monitor);
      this.checkIfProjectHasAppFile();
      _xblockexpression = result;
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
                _and = _isOnSourcePath;
              }
              if (_and) {
                RebarBuilder.this.foundAppSrc = true;
              }
            }
            _xblockexpression = (!RebarBuilder.this.foundAppSrc);
          }
          return _xblockexpression;
        }
      };
      _project.accept(_function);
      IMarker _xifexpression = null;
      if ((!this.foundAppSrc)) {
        IProject _project_1 = this.getProject();
        _xifexpression = MarkerUtils.createProblemMarker(_project_1, null, "No .app.src file found, can\'t compile with rebar", (-1), 
          IMarker.SEVERITY_WARNING);
      }
      _xblockexpression = _xifexpression;
    }
    return _xblockexpression;
  }
  
  public BuilderProperties getProperties() {
    return null;
  }
}
