package org.erlide.core.internal.builder;

import java.io.IOException;

import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.ProjectConfigurator;

import com.google.common.base.Preconditions;

public abstract class ProjectConfigurationPersister {

    private final ProjectConfigurator configurator;
    private final IProject project;

    public ProjectConfigurationPersister(@NonNull final IProject project,
            @NonNull final ProjectConfigurator configurator) {
        Preconditions.checkNotNull(project);
        Preconditions.checkNotNull(configurator);
        this.project = project;
        this.configurator = configurator;
    }

    public IProject getProject() {
        return project;
    }

    public ProjectConfigurator getConfigurator() {
        return configurator;
    }

    public abstract ErlangProjectProperties getConfiguration() throws IOException;

    public abstract void setConfiguration(final ErlangProjectProperties info)
            throws IOException;

}
