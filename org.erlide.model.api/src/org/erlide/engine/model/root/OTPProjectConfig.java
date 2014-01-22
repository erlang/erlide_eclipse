package org.erlide.engine.model.root;

public class OTPProjectConfig extends ProjectConfig {

    @Override
    public ProjectConfigurator getConfigurator() {
        return null;
    }

    @Override
    public ErlangProjectProperties getConfiguration() {
        return ErlangProjectProperties.DEFAULT;
    }

    @Override
    public void setConfiguration(final ErlangProjectProperties info) {
    }

}
