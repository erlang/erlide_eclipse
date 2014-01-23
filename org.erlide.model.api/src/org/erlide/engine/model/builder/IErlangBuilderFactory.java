package org.erlide.engine.model.builder;

import java.io.File;

import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.ProjectConfigurator;

public interface IErlangBuilderFactory {

    ErlangBuilder getBuilder(BuilderTool tool);

    ProjectConfigurator getConfig(BuilderConfigType config, IErlProject project);

    ProjectConfigurator getConfig(BuilderConfigType config, File directory);

}
