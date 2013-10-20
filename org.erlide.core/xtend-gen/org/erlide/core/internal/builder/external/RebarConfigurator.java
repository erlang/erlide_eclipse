package org.erlide.core.internal.builder.external;

import org.eclipse.core.resources.IProject;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.erlide.core.internal.builder.BuilderConfigurator;
import org.erlide.engine.model.root.IErlangProjectProperties;

@SuppressWarnings("all")
public class RebarConfigurator implements BuilderConfigurator {
  public String encodeConfig(final IProject project, final IErlangProjectProperties info) {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append("%% coding: utf-8");
    _builder.newLine();
    return _builder.toString();
  }
  
  public IErlangProjectProperties decodeConfig(final String config) {
    UnsupportedOperationException _unsupportedOperationException = new UnsupportedOperationException("TODO: auto-generated method stub");
    throw _unsupportedOperationException;
  }
  
  public String getConfigFile() {
    return "rebar.config";
  }
}
