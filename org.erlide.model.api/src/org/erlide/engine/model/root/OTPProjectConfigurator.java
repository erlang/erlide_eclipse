package org.erlide.engine.model.root;

public class OTPProjectConfigurator implements ProjectConfigurator {

    @Override
    public ProjectConfigurationSerializer getSerializer() {
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
