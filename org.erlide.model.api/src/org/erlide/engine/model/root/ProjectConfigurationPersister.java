package org.erlide.engine.model.root;

import java.io.IOException;

import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.annotation.NonNull;

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

    @SuppressWarnings("null")
    @NonNull
    public IProject getProject() {
        return project;
    }

    public ProjectConfigurator getConfigurator() {
        return configurator;
    }

    public abstract ErlangProjectProperties getConfiguration() throws IOException;

    public abstract void setConfiguration(@NonNull final ErlangProjectProperties info)
            throws IOException;

}
