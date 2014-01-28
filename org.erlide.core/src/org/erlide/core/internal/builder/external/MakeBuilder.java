package org.erlide.core.internal.builder.external;

import org.erlide.core.ErlangCore;
import org.erlide.core.internal.builder.ExternalBuilder;
import org.erlide.engine.model.builder.BuilderProperties;
import org.erlide.engine.model.builder.BuilderTool;

public class MakeBuilder extends ExternalBuilder {

    public MakeBuilder() {
        super(BuilderTool.MAKE);
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
