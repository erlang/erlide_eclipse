package org.erlide.ui.wizards;

import org.eclipse.core.runtime.IPath;
import org.erlide.engine.model.root.ErlangProjectProperties;

@SuppressWarnings("all")
public class NewProjectData extends ErlangProjectProperties {
  private String _builderName;
  
  public String getBuilderName() {
    return this._builderName;
  }
  
  public void setBuilderName(final String builderName) {
    this._builderName = builderName;
  }
  
  private String _builderConfigName;
  
  public String getBuilderConfigName() {
    return this._builderConfigName;
  }
  
  public void setBuilderConfigName(final String builderConfigName) {
    this._builderConfigName = builderConfigName;
  }
  
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
}
