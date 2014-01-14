package org.erlide.engine.model.builder;

import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.ProjectConfig;

public interface IErlangBuilderFactory {

    ErlangBuilder getBuilder(BuilderTool tool);

    ProjectConfig getConfig(BuilderConfigType config, IErlProject project);

}
