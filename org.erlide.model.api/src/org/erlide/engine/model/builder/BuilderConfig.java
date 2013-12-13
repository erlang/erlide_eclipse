package org.erlide.engine.model.builder;

import org.erlide.engine.model.root.ProjectConfigurationPersister;

public class BuilderConfig {

    private final ProjectConfigurationPersister persister;

    public BuilderConfig(final ProjectConfigurationPersister persister) {
        this.persister = persister;
    }

    public ProjectConfigurationPersister getPersister() {
        return persister;
    }

}
