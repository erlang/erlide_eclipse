package org.erlide.core.internal.builder.external;

import org.eclipse.core.resources.IProject;
import org.erlide.core.internal.builder.FileProjectConfigurationPersister;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.ProjectConfigurationPersister;
import org.erlide.engine.model.root.ProjectConfigurator;

@SuppressWarnings("all")
public class RebarConfigurator implements ProjectConfigurator {
  public String encodeConfig(final IProject project, final ErlangProjectProperties info) {
    return null;
  }
  
  public ErlangProjectProperties decodeConfig(final String config) {
    return null;
  }
  
  public ProjectConfigurationPersister getPersister(final IProject project) {
    FileProjectConfigurationPersister _fileProjectConfigurationPersister = new FileProjectConfigurationPersister(project, this, "rebar.config");
    return _fileProjectConfigurationPersister;
  }
}
