package org.erlide.testing.utils

import org.eclipse.core.resources.IMarker
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.IncrementalProjectBuilder
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.NullProgressMonitor
import org.erlide.engine.model.builder.MarkerUtils
import org.erlide.engine.model.erlang.IErlModule
import org.erlide.engine.model.root.IErlProject

class ProjectBuilder {

    IErlProject project

    def void cleanProject() {
        // TODO project.clean(new NullProgressMonitor())
    }

    def void fullProjectBuild() throws CoreException {
        project.workspaceProject.build(IncrementalProjectBuilder.FULL_BUILD, new NullProgressMonitor())
    }

    def IMarker[] allBuildErrorsOf(IErlModule unit) throws CoreException {
        return unit.resource.findMarkers(MarkerUtils.PROBLEM_MARKER, true, IResource.DEPTH_INFINITE)
    }
}
