package org.erlide.engine.model.root;

import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.annotation.NonNull;

public interface ProjectConfigurator {

    /**
     * Encode the project properties in a string (as it would look like in the
     * config file).
     */
    String encodeConfig(@NonNull IProject project, @NonNull ErlangProjectProperties info);

    /**
     * Decode project properties from string (as read from config file).
     * 
     * TODO: store encoding
     * 
     * TODO: store config text as template to use for encoding
     */
    ErlangProjectProperties decodeConfig(@NonNull String config);

}
