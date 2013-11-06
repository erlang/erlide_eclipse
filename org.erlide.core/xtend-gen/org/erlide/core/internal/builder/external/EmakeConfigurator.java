package org.erlide.core.internal.builder.external;

import org.eclipse.core.resources.IProject;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.ProjectConfigurator;

@SuppressWarnings("all")
public class EmakeConfigurator implements ProjectConfigurator {
  public String encodeConfig(final IProject project, final ErlangProjectProperties info) {
    return null;
  }
  
  public ErlangProjectProperties decodeConfig(final String config) {
    ErlangProjectProperties _xblockexpression = null;
    {
      ErlangProjectProperties _erlangProjectProperties = new ErlangProjectProperties();
      final ErlangProjectProperties result = _erlangProjectProperties;
      _xblockexpression = (result);
    }
    return _xblockexpression;
  }
}
