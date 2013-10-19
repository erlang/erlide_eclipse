package org.erlide.core.internal.builder.external;

import org.eclipse.core.resources.IProject;
import org.erlide.core.internal.builder.BuilderConfigurator;
import org.erlide.engine.model.root.BuilderConfigParser;
import org.erlide.engine.model.root.IErlangProjectProperties;

public class MakeConfigurator implements BuilderConfigurator {

    @Override
    public void createConfig(final IProject project, final IErlangProjectProperties info) {
        // do nothing, creating a generic Makefile is too difficult
        // maybe if users demand it
    }

    @Override
    public BuilderConfigParser getConfigParser() {
        // do nothing at the moment
        return null;
    }

}
