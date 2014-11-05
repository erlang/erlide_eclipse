package org.erlide.testing.utils;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.erlide.engine.model.builder.MarkerUtils;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlProject;

@SuppressWarnings("all")
public class ProjectBuilder {
  private IErlProject project;
  
  public void cleanProject() {
  }
  
  public void fullProjectBuild() throws CoreException {
    IProject _workspaceProject = this.project.getWorkspaceProject();
    NullProgressMonitor _nullProgressMonitor = new NullProgressMonitor();
    _workspaceProject.build(IncrementalProjectBuilder.FULL_BUILD, _nullProgressMonitor);
  }
  
  public IMarker[] allBuildErrorsOf(final IErlModule unit) throws CoreException {
    IResource _resource = unit.getResource();
    return _resource.findMarkers(MarkerUtils.PROBLEM_MARKER, true, IResource.DEPTH_INFINITE);
  }
}
