package org.erlide.engine.model.builder;

import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.ProjectConfigurationPersister;
import org.erlide.engine.model.root.ProjectConfigurator;

public abstract class BuilderConfig {

    private final BuilderConfigType type;
    private final IErlProject project;
    private final ProjectConfigurator configurator;

    public BuilderConfig(final BuilderConfigType type, final IErlProject project,
            final ProjectConfigurator configurator) {
        this.type = type;
        this.project = project;
        this.configurator = configurator;
    }

    public BuilderConfigType getType() {
        return type;
    }

    public IErlProject getProject() {
        return project;
    }

    public abstract ProjectConfigurationPersister getPersister();

    public ProjectConfigurator getConfigurator() {
        return configurator;
    }

}
