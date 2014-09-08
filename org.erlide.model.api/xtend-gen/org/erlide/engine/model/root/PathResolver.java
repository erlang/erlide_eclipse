package org.erlide.engine.model.root;

import com.google.common.collect.Lists;
import java.net.URI;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import org.eclipse.core.filesystem.URIUtil;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;

@SuppressWarnings("all")
public class PathResolver {
  public Collection<IPath> resolvePaths(final Collection<IPath> paths) {
    IWorkspace _workspace = ResourcesPlugin.getWorkspace();
    final IPathVariableManager pathVariableManager = _workspace.getPathVariableManager();
    int _size = paths.size();
    final List<IPath> result = Lists.<IPath>newArrayListWithCapacity(_size);
    for (final IPath path : paths) {
      {
        URI _uRI = URIUtil.toURI(path);
        URI _resolveURI = pathVariableManager.resolveURI(_uRI);
        final IPath resolvedPath = URIUtil.toPath(_resolveURI);
        result.add(resolvedPath);
      }
    }
    return Collections.<IPath>unmodifiableCollection(result);
  }
  
  public IPath resolvePath(final IPath path) {
    IWorkspace _workspace = ResourcesPlugin.getWorkspace();
    final IPathVariableManager pathVariableManager = _workspace.getPathVariableManager();
    URI _uRI = URIUtil.toURI(path);
    URI _resolveURI = pathVariableManager.resolveURI(_uRI);
    return URIUtil.toPath(_resolveURI);
  }
}
