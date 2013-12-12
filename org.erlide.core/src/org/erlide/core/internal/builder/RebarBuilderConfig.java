package org.erlide.core.internal.builder;

import org.erlide.core.internal.builder.external.RebarConfigurator;
import org.erlide.engine.model.builder.BuilderConfig;
import org.erlide.engine.model.builder.BuilderConfigType;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.ProjectConfigurationPersister;

public class RebarBuilderConfig extends BuilderConfig {

    public RebarBuilderConfig(final IErlProject project) {
        super(BuilderConfigType.REBAR, project, new RebarConfigurator());
    }

    @Override
    public ProjectConfigurationPersister getPersister() {
        return new FileProjectConfigurationPersister(getConfigurator(), getType()
                .getConfigName());
    }

}
