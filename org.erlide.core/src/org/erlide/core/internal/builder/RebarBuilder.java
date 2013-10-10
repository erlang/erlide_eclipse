package org.erlide.core.internal.builder;

import org.erlide.engine.model.root.BuilderConfigParser;

public class RebarBuilder extends ExternalBuilder {

    @Override
    public String getOsCommand() {
        return "rebar";
    }

    @Override
    public BuilderConfigParser getConfigParser() {
        // TODO Auto-generated method stub
        return null;
    }

}
