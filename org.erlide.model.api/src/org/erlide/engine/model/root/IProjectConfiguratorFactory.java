package org.erlide.engine.model.root;

import java.io.File;

public interface IProjectConfiguratorFactory {

    public abstract IProjectConfigurator getConfig(ProjectConfigType configType,
            IErlProject project);

    public abstract IProjectConfigurator getConfig(ProjectConfigType configType,
            File directory);

}
