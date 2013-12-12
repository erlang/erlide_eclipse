package org.erlide.engine.model.builder;

import org.erlide.engine.model.root.ProjectConfigurationPersister;

public interface IErlangBuilderFactory {

    ErlangBuilder getBuilder(BuilderTool tool);

    ProjectConfigurationPersister getConfigurationPersister(BuilderConfigType info);

}
