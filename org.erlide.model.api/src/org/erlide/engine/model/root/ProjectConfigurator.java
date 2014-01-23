package org.erlide.engine.model.root;

public interface ProjectConfigurator {

    ProjectConfigurationSerializer getConfigurator();

    ErlangProjectProperties getConfiguration();

    void setConfiguration(final ErlangProjectProperties info);

}
