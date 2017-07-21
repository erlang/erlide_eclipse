package org.erlide.core.internal.builder.external;

import org.erlide.core.internal.builder.ExternalBuilder;
import org.erlide.engine.model.builder.BuilderProperties;
import org.erlide.engine.model.builder.BuilderTool;

public class MakeBuilder extends ExternalBuilder {

    public MakeBuilder() {
        super(BuilderTool.MAKE);
    }

    @Override
    public BuilderProperties getProperties() {
        return null;
    }

}
