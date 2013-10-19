package org.erlide.core.internal.builder;

import org.eclipse.core.resources.IProject;
import org.erlide.engine.model.root.BuilderConfigParser;
import org.erlide.engine.model.root.IErlangProjectProperties;

public class InternalConfigurator implements BuilderConfigurator {

    @Override
    public void createConfig(final IProject project, final IErlangProjectProperties info) {
        // nothing to do here
    }

    @Override
    public BuilderConfigParser getConfigParser() {
        // nothing to do here
        return null;
    }

}
