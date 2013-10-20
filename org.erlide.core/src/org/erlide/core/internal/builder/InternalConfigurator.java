package org.erlide.core.internal.builder;

import org.eclipse.core.resources.IProject;
import org.erlide.engine.model.root.IErlangProjectProperties;

public class InternalConfigurator implements BuilderConfigurator {

    @Override
    public String encodeConfig(final IProject project, final IErlangProjectProperties info) {
        // nothing to do here
        return null;
    }

    @Override
    public IErlangProjectProperties decodeConfig(final String config) {
        // nothing to do here
        return null;
    }

    @Override
    public String getConfigFile() {
        // none
        return null;
    }
}
