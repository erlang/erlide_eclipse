package org.erlide.engine.model.builder;

public enum BuilderInfo {
    INTERNAL("org.erlide.core"), MAKE("Makefile"), EMAKE("Emakefile"), REBAR(
            "rebar.config");

    private final String configName;

    BuilderInfo(final String configName) {
        this.configName = configName;
    }

    public String getConfigName() {
        return configName;
    }
}
