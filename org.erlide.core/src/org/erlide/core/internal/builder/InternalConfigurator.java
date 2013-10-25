package org.erlide.core.internal.builder;

import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.ProjectConfigurator;

public class InternalConfigurator implements ProjectConfigurator {

    @Override
    public String encodeConfig(@NonNull final IProject project,
            @NonNull final ErlangProjectProperties info) {
        // nothing to do here
        return null;
    }

    @Override
    public ErlangProjectProperties decodeConfig(@NonNull final String config) {
        // nothing to do here
        return null;
    }

}
