package org.erlide.engine.model.builder;

public enum BuilderInfo {
    INTERNAL(null), MAKE("Makefile"), EMAKE("Emakefile"), REBAR("rebar.config");

    private final String configFile;

    BuilderInfo(final String configFile) {
        this.configFile = configFile;
    }

    public String getConfigFile() {
        return configFile;
    }
}
