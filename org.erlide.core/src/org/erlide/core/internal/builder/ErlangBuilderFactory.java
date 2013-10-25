package org.erlide.core.internal.builder;

import java.util.EnumMap;
import java.util.Map;

import org.erlide.core.internal.builder.external.EmakeBuilder;
import org.erlide.core.internal.builder.external.EmakeConfigurator;
import org.erlide.core.internal.builder.external.MakeBuilder;
import org.erlide.core.internal.builder.external.MakeConfigurator;
import org.erlide.core.internal.builder.external.RebarBuilder;
import org.erlide.core.internal.builder.external.RebarConfigurator;
import org.erlide.engine.model.builder.BuilderInfo;
import org.erlide.engine.model.builder.ErlangBuilder;
import org.erlide.engine.model.builder.IErlangBuilderFactory;
import org.erlide.engine.model.root.ProjectConfigurationPersister;
import org.erlide.util.ErlLogger;

public class ErlangBuilderFactory implements IErlangBuilderFactory {

    public ErlangBuilderFactory() {
    }

    @Override
    public ErlangBuilder getBuilder(final String name) {
        try {
            return getBuilder(BuilderInfo.valueOf(name));
        } catch (final IllegalArgumentException e) {
            ErlLogger.error("Bad builder requested: %s, returning INTERNAL.", name);
            return getBuilder(BuilderInfo.INTERNAL);
        }
    }

    private final Map<BuilderInfo, ErlangBuilder> builderMap = new EnumMap<BuilderInfo, ErlangBuilder>(
            BuilderInfo.class);

    public synchronized ErlangBuilder getBuilder(final BuilderInfo info) {
        ErlangBuilder builder = builderMap.get(info);
        if (builder == null) {
            switch (info) {
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
            default:
                builder = new InternalBuilder();
            }
            builder.setConfigurationPersister(getConfigurationPersister(info));
        }
        builderMap.put(info, builder);
        return builder;
    }

    private ProjectConfigurationPersister getConfigurationPersister(final BuilderInfo info) {
        switch (info) {
        case INTERNAL:
            return new PreferencesProjectConfigurationPersister(
                    new InternalConfigurator(), "org.erlide.core");
        case MAKE:
            return new FileProjectConfigurationPersister(new MakeConfigurator(),
                    "Makefile");
        case EMAKE:
            return new FileProjectConfigurationPersister(new EmakeConfigurator(),
                    "Emakefile");
        case REBAR:
            return new FileProjectConfigurationPersister(new RebarConfigurator(),
                    "rebar.config");
        }
        // doesn't happen
        return null;
    }

}
