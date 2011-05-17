/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *******************************************************************************/
package org.erlide.core.model.util;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.erlide.core.CoreScope;
import org.erlide.core.ErlangCore;
import org.erlide.core.backend.BackendUtils;
import org.erlide.core.model.root.IErlProject;

/**
 * Simple utility functions
 * 
 * 
 * @author Eric Merritt [cyberlync at yahoo dot com] Vlad Jakob C
 */
public class PluginUtils {
    /**
     * Displays an error that occured during the project creation. *
     * 
     * @param x
     *            details on the error
     * @return IStatus
     */
    public static IStatus makeStatus(final Exception x) {
        return new Status(IStatus.ERROR, ErlangCore.PLUGIN_ID, 0,
                x.getMessage(), x);
    }

    public static Set<IPath> getFullPaths(final IProject project,
            final Collection<IPath> sourcePaths) {
        final HashSet<IPath> result = new HashSet<IPath>();
        for (final IPath path : sourcePaths) {
            final String path_string = path.toString();
            if (path_string.equals(".")) {
                result.add(project.getFullPath());
            } else {
                result.add(project.getFolder(path).getFullPath());
            }
        }
        return result;
    }

    private static ContainerFilter getIncludePathFilter(final IProject project,
            final IContainer current) {
        final IErlProject erlProject = CoreScope.getModel().getErlangProject(
                project);
        return new ContainerFilter() {
            private final Set<IPath> paths = getFullPaths(project,
                    erlProject.getIncludeDirs());

            public boolean accept(final IContainer container) {
                return container.equals(current)
                        || paths.contains(container.getFullPath());
            }
        };
    }

    private static final class SourcePathContainerFilter implements
            ContainerFilter {
        private final Set<IPath> paths;
        private final Set<String> extra;

        SourcePathContainerFilter(final IProject project) {
            final IErlProject erlProject = CoreScope.getModel()
                    .getErlangProject(project);
            paths = getFullPaths(project, erlProject.getSourceDirs());
            extra = new HashSet<String>();
            extra.addAll(BackendUtils.getExtraSourcePathsForModel(project));
        }

        public boolean accept(final IContainer container) {
            return paths.contains(container.getFullPath())
                    || extra.contains(container.getLocation().toString());
        }
    }

    public static ContainerFilter getSourcePathFilter(final IProject project) {
        return new SourcePathContainerFilter(project);
    }

    public static ContainerFilterCreator getSourcePathFilterCreator() {
        return new ContainerFilterCreator() {

            public ContainerFilter createFilterForProject(final IProject project) {
                return getSourcePathFilter(project);
            }
        };
    }

    public static ContainerFilterCreator getIncludePathFilterCreator(
            final IContainer current) {
        return new ContainerFilterCreator() {

            public ContainerFilter createFilterForProject(final IProject project) {
                return getIncludePathFilter(project, current);
            }
        };
    }

    public static IPath resolvePVMPath(final IPathVariableManager pvm,
            final IPath path) {
        return pvm.resolvePath(path);
    }

    public static IPath getPVMValue(final IPathVariableManager pvm,
            final String name) {
        return pvm.getValue(name);
    }
}
