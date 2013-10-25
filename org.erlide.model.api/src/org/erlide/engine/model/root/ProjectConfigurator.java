package org.erlide.engine.model.root;

import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.annotation.NonNull;

public interface ProjectConfigurator {

    ProjectConfigurationPersister getPersister(@NonNull IProject project);

    /**
     * Encode the project properties in a string (as it would look like in the
     * config file).
     */
    String encodeConfig(@NonNull IProject project, @NonNull ErlangProjectProperties info);

    /**
     * Decode project properties from string (as read from config file).
     */
    ErlangProjectProperties decodeConfig(@NonNull String config);

}
