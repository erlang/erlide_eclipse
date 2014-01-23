package org.erlide.engine.model.root;

public class OTPProjectConfig implements ProjectConfigurator {

    @Override
    public ProjectConfigurationSerializer getConfigurator() {
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
