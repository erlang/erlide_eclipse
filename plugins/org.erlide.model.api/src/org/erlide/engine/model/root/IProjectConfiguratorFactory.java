package org.erlide.engine.model.root;

import java.io.File;

public interface IProjectConfiguratorFactory {

    IProjectConfigurator getConfig(ProjectConfigType configType, IErlProject project);

    IProjectConfigurator getConfig(ProjectConfigType configType, File directory);

}
