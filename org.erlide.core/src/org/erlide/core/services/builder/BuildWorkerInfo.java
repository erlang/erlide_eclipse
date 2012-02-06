package org.erlide.core.services.builder;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;

public class BuildWorkerInfo {

    public IResource resource;
    public IProject project;

    public BuildWorkerInfo(final IProject project, final IResource resource) {
        this.project = project;
        this.resource = resource;
    }

}
