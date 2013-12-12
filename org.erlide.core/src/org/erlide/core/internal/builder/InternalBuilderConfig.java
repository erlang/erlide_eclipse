package org.erlide.core.internal.builder;

import org.erlide.engine.model.builder.BuilderConfig;
import org.erlide.engine.model.builder.BuilderConfigType;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.ProjectConfigurationPersister;

public class InternalBuilderConfig extends BuilderConfig {

    public InternalBuilderConfig(final IErlProject project) {
        super(BuilderConfigType.INTERNAL, project, null);
    }

    @Override
    public ProjectConfigurationPersister getPersister() {
        return new PreferencesProjectConfigurationPersister(getType().getConfigName());
    }

}
