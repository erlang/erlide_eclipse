package org.erlide.core.internal.builder;

import org.eclipse.core.resources.IProject;
import org.erlide.engine.model.root.BuilderConfigParser;
import org.erlide.engine.model.root.IErlangProjectProperties;

public interface BuilderConfigurator {

    void createConfig(IProject project, IErlangProjectProperties info);

    BuilderConfigParser getConfigParser();

}
