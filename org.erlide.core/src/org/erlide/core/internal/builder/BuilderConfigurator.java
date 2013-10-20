package org.erlide.core.internal.builder;

import org.eclipse.core.resources.IProject;
import org.erlide.engine.model.root.IErlangProjectProperties;

public interface BuilderConfigurator {

    /**
     * Path of config file relative to project. Null if not applicable.
     * 
     */
    String getConfigFile();

    /**
     * Encode the project properties in a string (as it would look like in the
     * config file).
     */
    String encodeConfig(IProject project, IErlangProjectProperties info);

    /**
     * Decode project properties from string (as read from config file).
     */
    IErlangProjectProperties decodeConfig(String config);

}
