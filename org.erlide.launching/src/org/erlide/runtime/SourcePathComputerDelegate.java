package org.erlide.runtime;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.sourcelookup.ISourceContainer;
import org.eclipse.debug.core.sourcelookup.ISourcePathComputerDelegate;
import org.eclipse.debug.core.sourcelookup.containers.ProjectSourceContainer;
import org.eclipse.debug.core.sourcelookup.containers.WorkspaceSourceContainer;
import org.erlide.runtime.backend.IErlangLaunchConfigurationAttributes;

public class SourcePathComputerDelegate implements ISourcePathComputerDelegate {

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.debug.internal.core.sourcelookup.ISourcePathComputerDelegate#computeSourceContainers(org.eclipse.debug.core.ILaunchConfiguration,
	 *      org.eclipse.core.runtime.IProgressMonitor)
	 */
	public ISourceContainer[] computeSourceContainers(
			final ILaunchConfiguration configuration,
			final IProgressMonitor monitor) throws CoreException {
		final String projectName = configuration.getAttribute(
				IErlangLaunchConfigurationAttributes.ATTR_PROJECT_NAME, "");
		// FIXME
		// configuration.getAttribute(IPDAConstants.ATTR_PDA_PROGRAM,
		// (String)null);
		ISourceContainer sourceContainer = null;
		if (projectName != null) {
			final IResource resource = ResourcesPlugin.getWorkspace().getRoot()
					.findMember(new Path(projectName));
			if (resource != null) {
				if (resource.getType() == IResource.PROJECT) {
					sourceContainer = new ProjectSourceContainer(
							(IProject) resource, false);
				}
			}
		}
		if (sourceContainer == null) {
			sourceContainer = new WorkspaceSourceContainer();
		}
		return new ISourceContainer[] { sourceContainer };
	}

}
