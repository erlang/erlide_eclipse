package org.erlide.engine.model.root;

public interface IProjectConfigurator {

    ErlangProjectProperties getConfiguration();

    void setConfiguration(final ErlangProjectProperties info);

}
