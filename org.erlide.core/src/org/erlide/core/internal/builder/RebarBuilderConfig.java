package org.erlide.core.internal.builder;

import org.erlide.core.internal.builder.external.RebarConfigurator;
import org.erlide.engine.model.builder.BuilderConfig;
import org.erlide.engine.model.builder.BuilderConfigType;
import org.erlide.engine.model.root.ProjectConfigurationPersister;

public class RebarBuilderConfig extends BuilderConfig {

    public RebarBuilderConfig() {
        super(BuilderConfigType.REBAR, new RebarConfigurator());
    }

    @Override
    public ProjectConfigurationPersister getPersister() {
        return new FileProjectConfigurationPersister(getConfigurator(), getType()
                .getConfigName());
    }

}
