package org.erlide.core.common;

import java.util.Collection;

import org.eclipse.core.runtime.IPath;

public interface SourcePathProvider {

    /*
     * TODO specify if the paths are resolved or not
     */
    Collection<IPath> getSourcePaths();

}
