package org.erlide.engine.model.root;

public abstract class ProjectConfigurationPersister {

    public abstract ProjectConfigurator getConfigurator();

    public abstract ErlangProjectProperties getConfiguration();

    public abstract void setConfiguration(final ErlangProjectProperties info);

}
