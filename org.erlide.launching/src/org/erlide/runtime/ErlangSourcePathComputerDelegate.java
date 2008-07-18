package org.erlide.runtime;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.sourcelookup.ISourceContainer;
import org.eclipse.debug.core.sourcelookup.ISourcePathComputerDelegate;
import org.eclipse.debug.core.sourcelookup.containers.ProjectSourceContainer;
import org.eclipse.debug.core.sourcelookup.containers.WorkspaceSourceContainer;
import org.erlide.runtime.backend.BackendUtil;
import org.erlide.runtime.backend.IErlangLaunchConfigurationAttributes;

public class ErlangSourcePathComputerDelegate implements
		ISourcePathComputerDelegate {

	public ISourceContainer[] computeSourceContainers(
			final ILaunchConfiguration configuration,
			final IProgressMonitor monitor) throws CoreException {
		final String projectName = configuration.getAttribute(
				IErlangLaunchConfigurationAttributes.ATTR_PROJECT_NAME,
				(String) null);
		final List<ISourceContainer> containers = new ArrayList<ISourceContainer>();
		if (projectName != null) {
			final IWorkspaceRoot root = ResourcesPlugin.getWorkspace()
					.getRoot();
			final IProject project = root.getProject(projectName);
			if (project != null) {
				containers.add(new ProjectSourceContainer(project, false));
			}
		}
		final String otherProjectNames = configuration.getAttribute(
				IErlangLaunchConfigurationAttributes.ATTR_OTHER_PROJECTS, "");
		final IProject[] otherProjects = BackendUtil
				.getProjects(otherProjectNames);
		for (final IProject p : otherProjects) {
			containers.add(new ProjectSourceContainer(p, false));
		}
		if (containers.isEmpty()) {
			containers.add(new WorkspaceSourceContainer());
		}
		return containers.toArray(new ISourceContainer[containers.size()]);
	}

}
