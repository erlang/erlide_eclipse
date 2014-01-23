package org.erlide.engine.model.root;

public interface ProjectConfigurator {

    ProjectConfigurationSerializer getSerializer();

    ErlangProjectProperties getConfiguration();

    void setConfiguration(final ErlangProjectProperties info);

}
