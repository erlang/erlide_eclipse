package org.erlide.selfhost.core;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.sourcelookup.ISourceContainer;
import org.eclipse.debug.core.sourcelookup.ISourcePathComputerDelegate;
import org.eclipse.jdt.launching.sourcelookup.containers.JavaSourcePathComputer;
import org.erlide.runtime.ErlangSourcePathComputerDelegate;

public class EclipseErlideSourcePathComputerDelegate implements
		ISourcePathComputerDelegate {

	public ISourceContainer[] computeSourceContainers(
			final ILaunchConfiguration configuration,
			final IProgressMonitor monitor) throws CoreException {
		final List<ISourceContainer> containers = new ArrayList<ISourceContainer>();

		ErlangSourcePathComputerDelegate erlDelegate = new ErlangSourcePathComputerDelegate();
		ISourceContainer[] erls = erlDelegate.computeSourceContainers(
				configuration, monitor);
		containers.addAll(Arrays.asList(erls));

		JavaSourcePathComputer javaDelegate = new JavaSourcePathComputer();
		ISourceContainer[] javas = javaDelegate.computeSourceContainers(
				configuration, monitor);
		containers.addAll(Arrays.asList(javas));

		return containers.toArray(new ISourceContainer[containers.size()]);
	}

}
