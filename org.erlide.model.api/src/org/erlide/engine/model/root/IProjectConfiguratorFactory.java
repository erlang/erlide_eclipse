package org.erlide.engine.model.root;

import java.io.File;

public interface IProjectConfiguratorFactory {

    public abstract ProjectConfigurator getConfig(ProjectConfigType configType,
            IErlProject project);

    public abstract ProjectConfigurator getConfig(ProjectConfigType configType,
            File directory);

}
