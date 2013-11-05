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
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;
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
        if (info == null) {
            return null;
        }
        String configName = info.getConfigName();
        switch (info) {
        case INTERNAL:
            return new PreferencesProjectConfigurationPersister(configName);
        case MAKE:
            return new FileProjectConfigurationPersister(new MakeConfigurator(),
                    configName);
        case EMAKE:
            return new FileProjectConfigurationPersister(new EmakeConfigurator(),
                    configName);
        case REBAR:
            return new FileProjectConfigurationPersister(new RebarConfigurator(),
                    configName);
        }
        // doesn't happen
        throw new IllegalArgumentException("Illegal Erlang builder: " + info.toString());
    }

    @Override
    public ErlangProjectProperties getConfig(final IErlProject project) {
        final ProjectConfigurationPersister persister = getConfigurationPersister(ErlangNature
                .detectBuilder(project.getWorkspaceProject()));
        if (persister == null) {
            return null;
        }
        return persister.getConfiguration(project);
    }

}
