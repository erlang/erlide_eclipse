package org.erlide.engine.model.root;

public interface ProjectConfigurator {

    ErlangProjectProperties getConfiguration();

    void setConfiguration(final ErlangProjectProperties info);

}
