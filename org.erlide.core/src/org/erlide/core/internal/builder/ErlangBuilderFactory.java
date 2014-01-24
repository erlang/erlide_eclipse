package org.erlide.core.internal.builder;

import java.util.EnumMap;
import java.util.Map;

import org.erlide.core.internal.builder.external.EmakeBuilder;
import org.erlide.core.internal.builder.external.MakeBuilder;
import org.erlide.core.internal.builder.external.RebarBuilder;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.builder.ErlangBuilder;
import org.erlide.engine.model.builder.IErlangBuilderFactory;

public class ErlangBuilderFactory implements IErlangBuilderFactory {

    public ErlangBuilderFactory() {
    }

    private final Map<BuilderTool, ErlangBuilder> builderMap = new EnumMap<BuilderTool, ErlangBuilder>(
            BuilderTool.class);

    @Override
    public synchronized ErlangBuilder getBuilder(final BuilderTool info) {
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
        }
        builderMap.put(info, builder);
        return builder;
    }

}
