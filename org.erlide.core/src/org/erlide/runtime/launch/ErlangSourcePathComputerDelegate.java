package org.erlide.runtime.launch;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.sourcelookup.ISourceContainer;
import org.eclipse.debug.core.sourcelookup.ISourcePathComputerDelegate;
import org.eclipse.debug.core.sourcelookup.containers.ProjectSourceContainer;
import org.eclipse.debug.core.sourcelookup.containers.WorkspaceSourceContainer;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.util.BackendUtils;
import org.erlide.jinterface.backend.RuntimeInfo;

public class ErlangSourcePathComputerDelegate implements
        ISourcePathComputerDelegate {

    public ISourceContainer[] computeSourceContainers(
            final ILaunchConfiguration configuration,
            final IProgressMonitor monitor) throws CoreException {
        final List<ISourceContainer> containers = new ArrayList<ISourceContainer>();
        final String projectNames = configuration.getAttribute(
                ErlLaunchAttributes.PROJECTS, "");
        final IProject[] projects = BackendUtils.getProjects(projectNames);
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

}
