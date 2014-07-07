package org.erlide.engine.model.root;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;

import com.google.common.collect.Lists;

@SuppressWarnings("all")
public class PathResolver {
  public Collection<IPath> resolvePaths(final Collection<IPath> paths) {
    IWorkspace _workspace = ResourcesPlugin.getWorkspace();
    final IPathVariableManager pathVariableManager = _workspace.getPathVariableManager();
    int _size = paths.size();
    final List<IPath> result = Lists.<IPath>newArrayListWithCapacity(_size);
    for (final IPath path : paths) {
      {
        final IPath resolvedPath = pathVariableManager.resolvePath(path);
        result.add(resolvedPath);
      }
    }
    return Collections.<IPath>unmodifiableCollection(result);
  }
  
  public IPath resolvePath(final IPath path) {
    IWorkspace _workspace = ResourcesPlugin.getWorkspace();
    final IPathVariableManager pathVariableManager = _workspace.getPathVariableManager();
    return pathVariableManager.resolvePath(path);
  }
}
