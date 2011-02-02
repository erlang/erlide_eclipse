package org.erlide.runtime.launch;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.sourcelookup.ISourceContainer;
import org.eclipse.debug.core.sourcelookup.ISourcePathComputerDelegate;
import org.eclipse.debug.core.sourcelookup.containers.ProjectSourceContainer;
import org.eclipse.debug.core.sourcelookup.containers.WorkspaceSourceContainer;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.jinterface.backend.RuntimeInfo;

public class ErlangSourcePathComputerDelegate implements
        ISourcePathComputerDelegate {

    public ISourceContainer[] computeSourceContainers(
            final ILaunchConfiguration configuration,
            final IProgressMonitor monitor) throws CoreException {
        final List<ISourceContainer> containers = new ArrayList<ISourceContainer>();
        final String projectNames = configuration.getAttribute(
                ErlLaunchAttributes.PROJECTS, "");
        final IProject[] projects = getProjects(projectNames);
        for (final IProject p : projects) {
            containers.add(new ProjectSourceContainer(p, false));
        }
        if (containers.isEmpty()) {
            containers.add(new WorkspaceSourceContainer());
        }
        final String runtimeName = configuration.getAttribute(
                ErlLaunchAttributes.RUNTIME_NAME, "").trim();
        final RuntimeInfo info = ErlangCore.getRuntimeInfoManager().getRuntime(
                runtimeName);
        containers
                .add(new ErlangOtpSourceContainer(new Path(info.getOtpHome())));
        return containers.toArray(new ISourceContainer[containers.size()]);
    }

    private static IProject[] getProjects(final String attribute) {
        final String[] projectNames = attribute.split(";");
        return getProjects(projectNames);
    }

    private static IProject[] getProjects(final String[] projectNames) {
        final List<IProject> projects = new ArrayList<IProject>();
        final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        for (final String s : projectNames) {
            if (s != null && s.length() > 0) {
                final IProject p = root.getProject(s);
                if (p != null) {
                    projects.add(p);
                }
            }
        }
        return projects.toArray(new IProject[projects.size()]);
    }

}
