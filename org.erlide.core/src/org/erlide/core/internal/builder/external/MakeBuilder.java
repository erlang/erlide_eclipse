package org.erlide.core.internal.builder.external;

import org.erlide.core.ErlangCore;
import org.erlide.core.internal.builder.ExternalBuilder;
import org.erlide.engine.model.builder.BuilderProperties;

public class MakeBuilder extends ExternalBuilder {

    @Override
    public String getOsCommand() {
        return "make";
    }

    @Override
    public String getId() {
        return ErlangCore.PLUGIN_ID + ".make.builder";
    }

    @Override
    public BuilderProperties getProperties() {
        // TODO Auto-generated method stub
        return null;
    }

}
