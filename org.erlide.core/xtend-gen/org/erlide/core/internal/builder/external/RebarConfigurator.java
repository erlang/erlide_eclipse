package org.erlide.core.internal.builder.external;

import org.eclipse.core.resources.IProject;
import org.erlide.core.internal.builder.BuilderConfigurator;
import org.erlide.engine.model.root.ErlangProjectProperties;

@SuppressWarnings("all")
public class RebarConfigurator implements BuilderConfigurator {
  public String encodeConfig(final IProject project, final ErlangProjectProperties info) {
    return null;
  }
  
  public ErlangProjectProperties decodeConfig(final String config) {
    return null;
  }
  
  public String getConfigFile() {
    return "rebar.config";
  }
}
