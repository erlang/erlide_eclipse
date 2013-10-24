package org.erlide.engine.model.root;

import org.eclipse.core.resources.IProject;

public interface ProjectConfigurator {

    /**
     * Encode the project properties in a string (as it would look like in the
     * config file).
     */
    String encodeConfig(IProject project, ErlangProjectProperties info);

    /**
     * Decode project properties from string (as read from config file).
     */
    ErlangProjectProperties decodeConfig(String config);

}
