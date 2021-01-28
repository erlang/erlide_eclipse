package org.erlide.engine.model.root;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.filesystem.URIUtil;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;

import com.google.common.collect.Lists;

@SuppressWarnings("all")
public class PathResolver {
    public Collection<IPath> resolvePaths(final Collection<IPath> paths) {
        final IPathVariableManager pathVariableManager = ResourcesPlugin.getWorkspace()
                .getPathVariableManager();
        final List<IPath> result = Lists.<IPath> newArrayListWithCapacity(paths.size());
        for (final IPath path : paths) {
            {
                final IPath resolvedPath = URIUtil
                        .toPath(pathVariableManager.resolveURI(URIUtil.toURI(path)));
                result.add(resolvedPath);
            }
        }
        return Collections.<IPath> unmodifiableCollection(result);
    }

    public IPath resolvePath(final IPath path) {
        final IPathVariableManager pathVariableManager = ResourcesPlugin.getWorkspace()
                .getPathVariableManager();
        return URIUtil.toPath(pathVariableManager.resolveURI(URIUtil.toURI(path)));
    }
}
