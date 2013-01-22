package org.erlide.utils;

import java.util.Collection;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;

public interface SourcePathProvider {

    /*
     * TODO specify if the paths are resolved or not
     */
    Collection<IPath> getSourcePathsForBuild(IProject project);

    Collection<IPath> getSourcePathsForModel(IProject project);

    Collection<IPath> getSourcePathsForExecution(IProject project);

    Collection<IPath> getIncludePaths(IProject project);
}
