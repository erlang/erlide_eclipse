package org.erlide.engine.model.root;

import com.google.common.base.Objects;
import java.util.Map;
import org.eclipse.core.runtime.IPath;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.engine.model.builder.BuilderConfigType;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.ErlangProjectProperties;

@SuppressWarnings("all")
public class NewProjectData extends ErlangProjectProperties {
  private String _name = null;
  
  public String getName() {
    return this._name;
  }
  
  public void setName(final String name) {
    this._name = name;
  }
  
  private IPath _location = null;
  
  public IPath getLocation() {
    return this._location;
  }
  
  public void setLocation(final IPath location) {
    this._location = location;
  }
  
  private boolean _existingProject = false;
  
  public boolean isExistingProject() {
    return this._existingProject;
  }
  
  public void setExistingProject(final boolean existingProject) {
    this._existingProject = existingProject;
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
  
  private Map<String,String> _builderData = CollectionLiterals.<String, String>newHashMap();
  
  public Map<String,String> getBuilderData() {
    return this._builderData;
  }
  
  public void setBuilderData(final Map<String,String> builderData) {
    this._builderData = builderData;
  }
  
  public String toString() {
    String _xblockexpression = null;
    {
      Objects.ToStringHelper _stringHelper = Objects.toStringHelper(this);
      final Procedure1<Objects.ToStringHelper> _function = new Procedure1<Objects.ToStringHelper>() {
        public void apply(final Objects.ToStringHelper it) {
          it.add("name", NewProjectData.this._name);
          it.add("location", NewProjectData.this._location);
          it.add("existingProject", NewProjectData.this._existingProject);
          it.add("builder", NewProjectData.this._builder);
          it.add("builderConfig", NewProjectData.this._builderConfig);
          it.add("builderData", NewProjectData.this._builderData);
          String _string = NewProjectData.super.toString();
          it.add("super", _string);
        }
      };
      final Objects.ToStringHelper helper = ObjectExtensions.<Objects.ToStringHelper>operator_doubleArrow(_stringHelper, _function);
      String _string = helper.toString();
      _xblockexpression = (_string);
    }
    return _xblockexpression;
  }
}
