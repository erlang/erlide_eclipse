package org.erlide.engine.model.root;

import org.eclipse.core.resources.IProject;

public abstract class ProjectConfigurationPersister {

    private IProject project;

    public IProject getProject() {
        return project;
    }

    public void setProject(final IProject project) {
        this.project = project;
    }

    public abstract ProjectConfigurator getConfigurator();

    public abstract ErlangProjectProperties getConfiguration(IErlProject erlProject);

    public abstract void setConfiguration(IErlProject erlProject,
            final ErlangProjectProperties info);

}
