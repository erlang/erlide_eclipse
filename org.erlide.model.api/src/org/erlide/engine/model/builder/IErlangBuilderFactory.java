package org.erlide.engine.model.builder;

import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.BuilderConfig;

public interface IErlangBuilderFactory {

    ErlangBuilder getBuilder(BuilderTool tool);

    BuilderConfig getConfig(BuilderConfigType config, IErlProject project);

}
