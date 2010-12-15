/*******************************************************************************
 * Copyright (c) 2004 Eric Merritt and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Eric Merritt
 *******************************************************************************/
package org.erlide.core.erlang.util;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IOldErlangProjectProperties;
import org.erlide.jinterface.util.ErlLogger;

import erlang.ErlideOpen;

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
        return new Status(IStatus.ERROR, ErlangPlugin.PLUGIN_ID, 0,
                x.getMessage(), x);
    }

    public static boolean isOnPaths(final IContainer con,
            final IProject project, final Collection<IPath> sourcePaths) {
        final IPath path = con.getFullPath();
        for (final IPath i : sourcePaths) {
            if (i.toString().equals(".")) {
                if (project.getFullPath().equals(path)) {
                    return true;
                }
            } else if (project.getFolder(i).getFullPath().equals(path)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Checks to see if the specified container is on the source path
     * 
     * @param container
     *            container to check
     * @return true if the container is on the project's source path
     */
    public static boolean isOnSourcePath(final IContainer container) {
        final IProject project = container.getProject();
        final IOldErlangProjectProperties prefs = ErlangCore
                .getProjectProperties(project);
        return isOnPaths(container, project, prefs.getSourceDirs());
    }

    /**
     * Checks if the specified container is on the include path
     * 
     * @param container
     *            container to check
     * @return true if the container is on the project's include path
     */
    public static boolean isOnIncludePath(final IContainer container) {
        final IProject project = container.getProject();
        final IOldErlangProjectProperties prefs = ErlangCore
                .getProjectProperties(project);
        return isOnPaths(container, project, prefs.getIncludeDirs());
    }

    public static Set<IPath> getFullPaths(final IProject project,
            final Collection<IPath> sourcePaths) {
        final HashSet<IPath> result = new HashSet<IPath>();
        for (final IPath i : sourcePaths) {
            if (i.equals(".")) {
                result.add(project.getFullPath());
            } else {
                result.add(project.getFolder(i).getFullPath());
            }
        }
        return result;
    }

    public static boolean isSourcePathParent(final IFolder con) {
        final IProject project = con.getProject();
        /*
         * Get the project settings so that we can find the source nodes
         */
        final IOldErlangProjectProperties prefs = ErlangCore
                .getProjectProperties(project);
        final Collection<IPath> sourcePaths = prefs.getSourceDirs();
        final IPath path = con.getFullPath();
        for (final IPath i : sourcePaths) {
            if (path.isPrefixOf(project.getFolder(i).getFullPath())) {
                return true;
            }
        }
        return false;
    }

    public static boolean isTracing(final String traceOption) {
        if (!Platform.inDebugMode()) {
            return false;
        }
        final String globalTraceValue = Platform
                .getDebugOption(ErlLogger.ERLIDE_GLOBAL_TRACE_OPTION);
        final String value = Platform
                .getDebugOption(ErlLogger.ERLIDE_GLOBAL_TRACE_OPTION + "/"
                        + traceOption);
        if (null != globalTraceValue && globalTraceValue.equals("true")
                && null != value && value.equals("true")) {
            return true;
        }
        return false;
    }

    private static ContainerFilter getIncludePathFilter(final IProject project,
            final IContainer current) {
        return new ContainerFilter() {
            private final Set<IPath> paths = getFullPaths(project, ErlangCore
                    .getProjectProperties(project).getIncludeDirs());

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
            paths = getFullPaths(project,
                    ErlangCore.getProjectProperties(project).getSourceDirs());
            extra = new HashSet<String>();
            extra.addAll(ErlideOpen.getExtraSourcePaths());
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

    public static boolean isOnExtraPath(final IContainer con) {
        final Collection<String> sourcePaths = ErlideOpen.getExtraSourcePaths();
        final String path = con.getLocation().toString();
        for (final String spath : sourcePaths) {
            if (path.equals(spath)) {
                return true;
            }
        }
        return false;
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
