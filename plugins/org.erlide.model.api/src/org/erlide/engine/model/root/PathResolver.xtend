package org.erlide.engine.model.root

import com.google.common.collect.Lists
import java.util.Collection
import java.util.Collections
import java.util.List
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.runtime.IPath
import org.eclipse.core.filesystem.URIUtil

class PathResolver {

    def Collection<IPath> resolvePaths(Collection<IPath> paths) {
        val pathVariableManager = ResourcesPlugin.getWorkspace().getPathVariableManager()
        val List<IPath> result = Lists.newArrayListWithCapacity(paths.size())
        for (IPath path : paths) {
            val resolvedPath = URIUtil.toPath(pathVariableManager.resolveURI(URIUtil.toURI(path)))
            result.add(resolvedPath)
        }
        return Collections.unmodifiableCollection(result)
    }

    def IPath resolvePath(IPath path) {
        val pathVariableManager = ResourcesPlugin.getWorkspace().getPathVariableManager()
        return URIUtil.toPath(pathVariableManager.resolveURI(URIUtil.toURI(path)))
    }

}
