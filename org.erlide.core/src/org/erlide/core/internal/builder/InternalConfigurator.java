package org.erlide.core.internal.builder;

import org.eclipse.core.resources.IProject;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.ProjectConfigurator;

public class InternalConfigurator implements ProjectConfigurator {

    @Override
    public String encodeConfig(final IProject project, final ErlangProjectProperties info) {
        // nothing to do here
        return null;
    }

    @Override
    public ErlangProjectProperties decodeConfig(final String config) {
        // nothing to do here
        return null;
    }

}
