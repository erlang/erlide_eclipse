package org.erlide.engine.model.root;

import java.io.IOException;

import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.annotation.NonNull;

import com.google.common.base.Preconditions;

public abstract class ProjectConfigurationPersister {

    private final ProjectConfigurator configurator;
    private IProject project;

    public ProjectConfigurationPersister(
            @NonNull final ProjectConfigurator configurator) {
        Preconditions.checkNotNull(configurator);
        this.configurator = configurator;
    }

    public IProject getProject() {
        return project;
    }

    public void setProject(final IProject project) {
        this.project = project;
    }

    public ProjectConfigurator getConfigurator() {
        return configurator;
    }

    public abstract ErlangProjectProperties getConfiguration(
            IErlProject erlProject) throws IOException;

    public abstract void setConfiguration(IErlProject erlProject,
            final ErlangProjectProperties info) throws IOException;

}
