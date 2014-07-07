package org.erlide.backend.launch;

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
import org.erlide.backend.BackendCore;
import org.erlide.backend.api.ErlRuntimeAttributes;
import org.erlide.backend.debug.model.ErlangOtpSourceContainer;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;

public class ErlangSourcePathComputerDelegate implements ISourcePathComputerDelegate {

    @Override
    public ISourceContainer[] computeSourceContainers(
            final ILaunchConfiguration configuration, final IProgressMonitor monitor)
            throws CoreException {
        final List<ISourceContainer> containers = new ArrayList<ISourceContainer>();
        final IProject[] projects = LaunchUtils
                .getErlangLaunchConfigurationProjects(configuration);
        for (final IProject p : projects) {
            containers.add(new ProjectSourceContainer(p, false));
        }
        if (containers.isEmpty()) {
            containers.add(new WorkspaceSourceContainer());
        }
        final String runtimeName = configuration.getAttribute(
                ErlRuntimeAttributes.RUNTIME_NAME, "").trim();
        final RuntimeInfo info = BackendCore.getRuntimeInfoCatalog().getRuntime(
                runtimeName);
        containers.add(new ErlangOtpSourceContainer(new Path(info.getOtpHome())));
        return containers.toArray(new ISourceContainer[containers.size()]);
    }

}
