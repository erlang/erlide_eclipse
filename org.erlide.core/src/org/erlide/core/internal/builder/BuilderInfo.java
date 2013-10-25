package org.erlide.core.internal.builder;

import org.erlide.core.internal.builder.external.EmakeBuilder;
import org.erlide.core.internal.builder.external.EmakeConfigurator;
import org.erlide.core.internal.builder.external.MakeBuilder;
import org.erlide.core.internal.builder.external.MakeConfigurator;
import org.erlide.core.internal.builder.external.RebarBuilder;
import org.erlide.core.internal.builder.external.RebarConfigurator;
import org.erlide.engine.model.root.ProjectConfigurationPersister;

public enum BuilderInfo {
    INTERNAL, MAKE, EMAKE, REBAR;

    private ErlangBuilder builder = null;
    private ProjectConfigurationPersister configurator;

    public synchronized ErlangBuilder getBuilder() {
        if (builder == null) {
            switch (this) {
            case INTERNAL:
                builder = new InternalBuilder();
                break;
            case MAKE:
                builder = new MakeBuilder();
                break;
            case EMAKE:
                builder = new EmakeBuilder();
                break;
            case REBAR:
                builder = new RebarBuilder();
                break;
            }
            builder.setConfigurationPersister(getConfiguratorPersister());
        }
        return builder;
    }

    public ProjectConfigurationPersister getConfiguratorPersister() {
        if (configurator == null) {
            switch (this) {
            case INTERNAL:
                configurator = new PreferencesProjectConfigurationPersister(
                        new InternalConfigurator(), "org.erlide.core");
                break;
            case MAKE:
                configurator = new FileProjectConfigurationPersister(
                        new MakeConfigurator(), "Makefile");
                break;
            case EMAKE:
                configurator = new FileProjectConfigurationPersister(
                        new EmakeConfigurator(), "Emakefile");
                break;
            case REBAR:
                configurator = new FileProjectConfigurationPersister(
                        new RebarConfigurator(), "rebar.config");
                break;
            }
        }
        return configurator;
    }
}
