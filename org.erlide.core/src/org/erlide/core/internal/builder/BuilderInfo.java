package org.erlide.core.internal.builder;

import org.erlide.core.internal.builder.external.EmakeBuilder;
import org.erlide.core.internal.builder.external.EmakeConfigurator;
import org.erlide.core.internal.builder.external.MakeBuilder;
import org.erlide.core.internal.builder.external.MakeConfigurator;
import org.erlide.core.internal.builder.external.RebarBuilder;
import org.erlide.core.internal.builder.external.RebarConfigurator;
import org.erlide.engine.model.root.ProjectConfigurator;

public enum BuilderInfo {
    INTERNAL, MAKE, EMAKE, REBAR;

    private ErlangBuilder builder = null;
    private ProjectConfigurator configurator = null;

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
            builder.setConfigurator(getConfigurator());
        }
        return builder;
    }

    public ProjectConfigurator getConfigurator() {
        if (configurator == null) {
            switch (this) {
            case INTERNAL:
                configurator = new InternalConfigurator();
                break;
            case MAKE:
                configurator = new MakeConfigurator();
                break;
            case EMAKE:
                configurator = new EmakeConfigurator();
                break;
            case REBAR:
                configurator = new RebarConfigurator();
                break;
            }
        }
        return configurator;
    }

}
