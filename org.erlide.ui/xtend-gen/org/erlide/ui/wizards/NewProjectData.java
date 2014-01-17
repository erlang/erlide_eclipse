package org.erlide.ui.wizards;

import org.eclipse.core.runtime.IPath;
import org.erlide.engine.model.builder.BuilderConfigType;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.ErlangProjectProperties;

@SuppressWarnings("all")
public class NewProjectData extends ErlangProjectProperties {
  private String _name;
  
  public String getName() {
    return this._name;
  }
  
  public void setName(final String name) {
    this._name = name;
  }
  
  private IPath _location;
  
  public IPath getLocation() {
    return this._location;
  }
  
  public void setLocation(final IPath location) {
    this._location = location;
  }
  
  private BuilderTool _builder = BuilderTool.INTERNAL;
  
  public BuilderTool getBuilder() {
    return this._builder;
  }
  
  public void setBuilder(final BuilderTool builder) {
    this._builder = builder;
  }
  
  private BuilderConfigType _builderConfig = BuilderConfigType.INTERNAL;
  
  public BuilderConfigType getBuilderConfig() {
    return this._builderConfig;
  }
  
  public void setBuilderConfig(final BuilderConfigType builderConfig) {
    this._builderConfig = builderConfig;
  }
}
