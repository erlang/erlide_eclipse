package org.erlide.engine.model.builder;

import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;

public interface IErlangBuilderFactory {

    ErlangBuilder getBuilder(String id);

    /**
     * Returns the detected configuration for the project. Returns null if
     * impossible (project doesn't exist or files not available).
     * 
     * @param project
     * @return
     */
    ErlangProjectProperties getConfig(IErlProject project);

}
