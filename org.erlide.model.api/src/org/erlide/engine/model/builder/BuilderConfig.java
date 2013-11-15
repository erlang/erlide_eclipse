package org.erlide.engine.model.builder;

public enum BuilderConfig {
    INTERNAL("org.erlide.core"), EMAKE("Emakefile"), REBAR("rebar.config");

    private final String configName;

    BuilderConfig(final String configName) {
        this.configName = configName;
    }

    /**
     * A string that points to where the configuration is stored for this
     * project. The configurator interprets the value as fit, it can be a file
     * name or a preference node name.
     */
    public String getConfigName() {
        return configName;
    }

}
