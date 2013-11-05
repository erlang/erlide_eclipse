package org.erlide.engine.model.builder;

import org.erlide.engine.model.root.ProjectConfigurationPersister;

public interface IErlangBuilderFactory {

    ErlangBuilder getBuilder(String id);

    ProjectConfigurationPersister getConfigurationPersister(BuilderConfig info);

}
