package org.erlide.engine.model.builder;

import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;

public interface IErlangBuilderFactory {

    ErlangBuilder getBuilder(String id);

    /**
     * Returns the BuilderInfo that applies for this project, if any, by
     * checking for the appropriate build file at the top level. If project
     * doesn't exist, returns null.
     * 
     * @param project
     * @return
     */
    BuilderInfo getBuilder(IErlProject project);

    /**
     * Returns the detected configuration for the project. Returns null if
     * impossible (project doesn't exist or files not available).
     * 
     * @param project
     * @return
     */
    ErlangProjectProperties getConfig(IErlProject project);

}
