package org.erlide.runtime;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.sourcelookup.ISourceContainer;
import org.eclipse.debug.core.sourcelookup.ISourcePathComputerDelegate;
import org.eclipse.debug.core.sourcelookup.containers.ProjectSourceContainer;
import org.eclipse.debug.core.sourcelookup.containers.WorkspaceSourceContainer;
import org.erlide.runtime.backend.BackendUtil;

public class ErlangSourcePathComputerDelegate implements
		ISourcePathComputerDelegate {

	public ISourceContainer[] computeSourceContainers(
			final ILaunchConfiguration configuration,
			final IProgressMonitor monitor) throws CoreException {
		final List<ISourceContainer> containers = new ArrayList<ISourceContainer>();
		final String projectNames = configuration.getAttribute(
				ErlLaunchAttributes.PROJECTS, "");
		final IProject[] projects = BackendUtil.getProjects(projectNames);
		for (final IProject p : projects) {
			containers.add(new ProjectSourceContainer(p, false));
		}
		if (containers.isEmpty()) {
			containers.add(new WorkspaceSourceContainer());
		}
		return containers.toArray(new ISourceContainer[containers.size()]);
	}

}
