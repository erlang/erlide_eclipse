package org.erlide.engine.model.builder;

import java.io.File;

import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.ProjectConfigType;
import org.erlide.engine.model.root.ProjectConfigurator;

public interface IErlangBuilderFactory {

    ErlangBuilder getBuilder(BuilderTool tool);

    ProjectConfigurator getConfig(ProjectConfigType config, IErlProject project);

    ProjectConfigurator getConfig(ProjectConfigType config, File directory);

}
