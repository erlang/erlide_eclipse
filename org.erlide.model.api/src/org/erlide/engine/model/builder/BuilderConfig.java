package org.erlide.engine.model.builder;

import org.erlide.engine.model.root.ProjectConfigurationPersister;
import org.erlide.engine.model.root.ProjectConfigurator;

public abstract class BuilderConfig {

    private final BuilderConfigType type;
    private final ProjectConfigurator configurator;

    public BuilderConfig(final BuilderConfigType type,
            final ProjectConfigurator configurator) {
        this.type = type;
        this.configurator = configurator;
    }

    public BuilderConfigType getType() {
        return type;
    }

    public abstract ProjectConfigurationPersister getPersister();

    public ProjectConfigurator getConfigurator() {
        return configurator;
    }

}
