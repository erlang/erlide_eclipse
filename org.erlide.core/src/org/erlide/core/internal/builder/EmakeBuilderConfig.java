package org.erlide.core.internal.builder;

import org.erlide.core.internal.builder.external.EmakeConfigurator;
import org.erlide.engine.model.builder.BuilderConfig;
import org.erlide.engine.model.builder.BuilderConfigType;
import org.erlide.engine.model.root.ProjectConfigurationPersister;

public class EmakeBuilderConfig extends BuilderConfig {

    public EmakeBuilderConfig() {
        super(BuilderConfigType.EMAKE, new EmakeConfigurator());
    }

    @Override
    public ProjectConfigurationPersister getPersister() {
        return new FileProjectConfigurationPersister(getConfigurator(), getType()
                .getConfigName());
    }

}
