package org.erlide.core.debug;

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
import org.erlide.core.backend.BackendCore;
import org.erlide.core.backend.ErlLaunchAttributes;
import org.erlide.core.backend.runtimeinfo.RuntimeInfo;
import org.erlide.core.model.util.CoreUtil;

public class ErlangSourcePathComputerDelegate implements
        ISourcePathComputerDelegate {

    public ISourceContainer[] computeSourceContainers(
            final ILaunchConfiguration configuration,
            final IProgressMonitor monitor) throws CoreException {
        final List<ISourceContainer> containers = new ArrayList<ISourceContainer>();
        final IProject[] projects = CoreUtil
                .getErlangLaunchConfigurationProjects(configuration);
        for (final IProject p : projects) {
            containers.add(new ProjectSourceContainer(p, false));
        }
        if (containers.isEmpty()) {
            containers.add(new WorkspaceSourceContainer());
        }
        final String runtimeName = configuration.getAttribute(
                ErlLaunchAttributes.RUNTIME_NAME, "").trim();
        final RuntimeInfo info = BackendCore.getRuntimeInfoManager()
                .getRuntime(runtimeName);
        containers
                .add(new ErlangOtpSourceContainer(new Path(info.getOtpHome())));
        return containers.toArray(new ISourceContainer[containers.size()]);
    }

}
