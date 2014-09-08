package org.erlide.engine.model.root;

import com.google.common.base.Objects;
import java.io.File;
import java.util.Collection;
import java.util.Map;
import org.eclipse.core.runtime.IPath;
import org.eclipse.xtend.lib.annotations.Accessors;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.InputOutput;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.Pure;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IProjectConfigurator;
import org.erlide.engine.model.root.IProjectConfiguratorFactory;
import org.erlide.engine.model.root.ProjectConfigType;
import org.erlide.engine.model.root.ProjectPreferencesConstants;
import org.erlide.runtime.api.RuntimeCore;
import org.erlide.runtime.runtimeinfo.IRuntimeInfoCatalog;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;

@Accessors
@SuppressWarnings("all")
public class NewProjectData extends ErlangProjectProperties {
  private String name = null;
  
  private IPath location = null;
  
  private boolean existingProject = false;
  
  private BuilderTool builder = BuilderTool.INTERNAL;
  
  private ProjectConfigType configType = ProjectConfigType.INTERNAL;
  
  private Map<String, String> builderData = CollectionLiterals.<String, String>newHashMap();
  
  private final IProjectConfiguratorFactory factory;
  
  public NewProjectData(final IProjectConfiguratorFactory factory) {
    this.factory = factory;
  }
  
  public String toString() {
    String _xblockexpression = null;
    {
      Objects.ToStringHelper _stringHelper = Objects.toStringHelper(this);
      final Procedure1<Objects.ToStringHelper> _function = new Procedure1<Objects.ToStringHelper>() {
        public void apply(final Objects.ToStringHelper it) {
          it.add("name", NewProjectData.this.name);
          it.add("location", NewProjectData.this.location);
          it.add("existingProject", NewProjectData.this.existingProject);
          it.add("configType", NewProjectData.this.configType);
          it.add("builder", NewProjectData.this.builder);
          it.add("builderData", NewProjectData.this.builderData);
          String _string = NewProjectData.super.toString();
          it.add("super", _string);
        }
      };
      final Objects.ToStringHelper helper = ObjectExtensions.<Objects.ToStringHelper>operator_doubleArrow(_stringHelper, _function);
      _xblockexpression = helper.toString();
    }
    return _xblockexpression;
  }
  
  public void loadFromFile() {
    IPath _location = this.getLocation();
    ProjectConfigType _configType = this.getConfigType();
    String _configName = _configType.getConfigName();
    IPath _append = _location.append(_configName);
    String _portableString = _append.toPortableString();
    final File f = new File(_portableString);
    boolean _exists = f.exists();
    if (_exists) {
      String _absolutePath = f.getAbsolutePath();
      String _plus = ("» LOAD " + _absolutePath);
      System.out.println(_plus);
      ProjectConfigType _configType_1 = this.getConfigType();
      IPath _location_1 = this.getLocation();
      String _portableString_1 = _location_1.toPortableString();
      File _file = new File(_portableString_1);
      final IProjectConfigurator config = this.factory.getConfig(_configType_1, _file);
      final ErlangProjectProperties props = config.getConfiguration();
      IPath _outputDir = props.getOutputDir();
      this.setOutputDir(_outputDir);
      Collection<IPath> _sourceDirs = props.getSourceDirs();
      this.setSourceDirs(_sourceDirs);
      Collection<IPath> _includeDirs = props.getIncludeDirs();
      this.setIncludeDirs(_includeDirs);
      Collection<IPath> _testDirs = props.getTestDirs();
      this.setTestDirs(_testDirs);
    }
  }
  
  public String detectProjectConfig() {
    String _xblockexpression = null;
    {
      InputOutput.<String>println("» DETECT builder config");
      String _xifexpression = null;
      boolean _tripleNotEquals = (this.location != null);
      if (_tripleNotEquals) {
        String _xblockexpression_1 = null;
        {
          InputOutput.<String>println("DETECT builder config");
          String _portableString = this.location.toPortableString();
          final File directory = new File(_portableString);
          String _xifexpression_1 = null;
          boolean _and = false;
          boolean _isDirectory = directory.isDirectory();
          if (!_isDirectory) {
            _and = false;
          } else {
            boolean _exists = directory.exists();
            _and = _exists;
          }
          if (_and) {
            String _xblockexpression_2 = null;
            {
              final IProjectConfigurator persister = this.factory.getConfig(this.configType, directory);
              InputOutput.<String>println(("PERSISTER " + persister));
              String _xifexpression_2 = null;
              boolean _tripleNotEquals_1 = (persister != null);
              if (_tripleNotEquals_1) {
                String _xblockexpression_3 = null;
                {
                  final ErlangProjectProperties props = persister.getConfiguration();
                  _xblockexpression_3 = InputOutput.<String>println(("detected PROPS: " + props));
                }
                _xifexpression_2 = _xblockexpression_3;
              }
              _xblockexpression_2 = _xifexpression_2;
            }
            _xifexpression_1 = _xblockexpression_2;
          }
          _xblockexpression_1 = _xifexpression_1;
        }
        _xifexpression = _xblockexpression_1;
      }
      _xblockexpression = _xifexpression;
    }
    return _xblockexpression;
  }
  
  public RuntimeInfo bestRuntime() {
    IRuntimeInfoCatalog _runtimeInfoCatalog = RuntimeCore.getRuntimeInfoCatalog();
    return _runtimeInfoCatalog.getRuntime(ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION, null);
  }
  
  @Pure
  public String getName() {
    return this.name;
  }
  
  public void setName(final String name) {
    this.name = name;
  }
  
  @Pure
  public IPath getLocation() {
    return this.location;
  }
  
  public void setLocation(final IPath location) {
    this.location = location;
  }
  
  @Pure
  public boolean isExistingProject() {
    return this.existingProject;
  }
  
  public void setExistingProject(final boolean existingProject) {
    this.existingProject = existingProject;
  }
  
  @Pure
  public BuilderTool getBuilder() {
    return this.builder;
  }
  
  public void setBuilder(final BuilderTool builder) {
    this.builder = builder;
  }
  
  @Pure
  public ProjectConfigType getConfigType() {
    return this.configType;
  }
  
  public void setConfigType(final ProjectConfigType configType) {
    this.configType = configType;
  }
  
  @Pure
  public Map<String, String> getBuilderData() {
    return this.builderData;
  }
  
  public void setBuilderData(final Map<String, String> builderData) {
    this.builderData = builderData;
  }
  
  @Pure
  public IProjectConfiguratorFactory getFactory() {
    return this.factory;
  }
}
