package org.erlide.engine.model.root;

import com.google.common.base.Objects;
import com.google.common.base.Strings;
import com.google.common.collect.Lists;
import java.util.ArrayList;
import java.util.Collection;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.xtend.lib.annotations.Accessors;
import org.eclipse.xtend.lib.annotations.EqualsHashCode;
import org.eclipse.xtend.lib.annotations.ToString;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.engine.model.root.ExternalKind;
import org.erlide.engine.model.root.PathSerializer;
import org.erlide.engine.model.root.ProjectPreferencesConstants;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.PreferencesUtils;

@Accessors
@EqualsHashCode
@ToString
@SuppressWarnings("all")
public class ErlangProjectProperties {
  private IPath outputDir;
  
  private Collection<IPath> sourceDirs;
  
  private Collection<IPath> includeDirs;
  
  private Collection<IPath> testDirs;
  
  private RuntimeVersion requiredRuntimeVersion;
  
  private String externalIncludesFile;
  
  private String externalModulesFile;
  
  public final static ErlangProjectProperties DEFAULT = ObjectExtensions.<ErlangProjectProperties>operator_doubleArrow(new ErlangProjectProperties(), new Procedure1<ErlangProjectProperties>() {
    public void apply(final ErlangProjectProperties it) {
      Collection<IPath> _unpackList = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS);
      it.sourceDirs = _unpackList;
      Path _path = new Path(ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR);
      it.outputDir = _path;
      Collection<IPath> _unpackList_1 = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS);
      it.includeDirs = _unpackList_1;
      Collection<IPath> _unpackList_2 = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_TEST_DIRS);
      it.testDirs = _unpackList_2;
      it.externalIncludesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES;
      it.externalModulesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES;
      it.requiredRuntimeVersion = ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION;
    }
  });
  
  public ErlangProjectProperties() {
    ArrayList<IPath> _newArrayList = CollectionLiterals.<IPath>newArrayList();
    this.sourceDirs = _newArrayList;
    Path _path = new Path("");
    this.outputDir = _path;
    ArrayList<IPath> _newArrayList_1 = CollectionLiterals.<IPath>newArrayList();
    this.includeDirs = _newArrayList_1;
    ArrayList<IPath> _newArrayList_2 = CollectionLiterals.<IPath>newArrayList();
    this.testDirs = _newArrayList_2;
    this.externalIncludesFile = "";
    this.externalModulesFile = "";
    this.requiredRuntimeVersion = ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION;
  }
  
  public void setSourceDirs(final Collection<IPath> dirs) {
    ArrayList<IPath> _newArrayList = Lists.<IPath>newArrayList(dirs);
    this.sourceDirs = _newArrayList;
  }
  
  public void setSourceDirs(final IPath... dirs) {
    ArrayList<IPath> _newArrayList = Lists.<IPath>newArrayList(dirs);
    this.sourceDirs = _newArrayList;
  }
  
  public void setIncludeDirs(final Collection<IPath> dirs) {
    ArrayList<IPath> _newArrayList = Lists.<IPath>newArrayList(dirs);
    this.includeDirs = _newArrayList;
  }
  
  public void setIncludeDirs(final IPath... dirs) {
    ArrayList<IPath> _newArrayList = Lists.<IPath>newArrayList(dirs);
    this.includeDirs = _newArrayList;
  }
  
  public void setTestDirs(final Collection<IPath> dirs) {
    ArrayList<IPath> _newArrayList = Lists.<IPath>newArrayList(dirs);
    this.testDirs = _newArrayList;
  }
  
  public void setTestDirs(final IPath... dirs) {
    ArrayList<IPath> _newArrayList = Lists.<IPath>newArrayList(dirs);
    this.testDirs = _newArrayList;
  }
  
  public void copyFrom(final ErlangProjectProperties props) {
    this.includeDirs = props.includeDirs;
    this.testDirs = props.testDirs;
    this.sourceDirs = props.sourceDirs;
    this.outputDir = props.outputDir;
    this.requiredRuntimeVersion = props.requiredRuntimeVersion;
    this.externalIncludesFile = props.externalIncludesFile;
    this.externalModulesFile = props.externalModulesFile;
  }
  
  public String getExternalIncludes() {
    final String externalIncludesString = this.getExternal(ExternalKind.EXTERNAL_INCLUDES);
    return externalIncludesString;
  }
  
  public String getExternalModules() {
    final String externalModulesString = this.getExternal(ExternalKind.EXTERNAL_MODULES);
    return externalModulesString;
  }
  
  private String getExternal(final ExternalKind external) {
    final IPreferencesService service = Platform.getPreferencesService();
    String _xifexpression = null;
    boolean _equals = Objects.equal(external, ExternalKind.EXTERNAL_INCLUDES);
    if (_equals) {
      _xifexpression = "default_external_includes";
    } else {
      _xifexpression = "default_external_modules";
    }
    final String key = _xifexpression;
    String result = this.getExternal(external, service, key, "org.erlide.ui");
    boolean _isNullOrEmpty = Strings.isNullOrEmpty(result);
    if (_isNullOrEmpty) {
      String _external = this.getExternal(external, service, key, "org.erlide.core");
      result = _external;
    }
    return result;
  }
  
  private String getExternal(final ExternalKind external, final IPreferencesService service, final String key, final String pluginId) {
    final String global = service.getString(pluginId, key, "", null);
    String _xifexpression = null;
    boolean _equals = Objects.equal(external, ExternalKind.EXTERNAL_INCLUDES);
    if (_equals) {
      _xifexpression = this.externalIncludesFile;
    } else {
      _xifexpression = this.externalModulesFile;
    }
    final String projprefs = _xifexpression;
    return PreferencesUtils.packArray(new String[] { projprefs, global });
  }
  
  @Pure
  public IPath getOutputDir() {
    return this.outputDir;
  }
  
  public void setOutputDir(final IPath outputDir) {
    this.outputDir = outputDir;
  }
  
  @Pure
  public Collection<IPath> getSourceDirs() {
    return this.sourceDirs;
  }
  
  @Pure
  public Collection<IPath> getIncludeDirs() {
    return this.includeDirs;
  }
  
  @Pure
  public Collection<IPath> getTestDirs() {
    return this.testDirs;
  }
  
  @Pure
  public RuntimeVersion getRequiredRuntimeVersion() {
    return this.requiredRuntimeVersion;
  }
  
  public void setRequiredRuntimeVersion(final RuntimeVersion requiredRuntimeVersion) {
    this.requiredRuntimeVersion = requiredRuntimeVersion;
  }
  
  @Pure
  public String getExternalIncludesFile() {
    return this.externalIncludesFile;
  }
  
  public void setExternalIncludesFile(final String externalIncludesFile) {
    this.externalIncludesFile = externalIncludesFile;
  }
  
  @Pure
  public String getExternalModulesFile() {
    return this.externalModulesFile;
  }
  
  public void setExternalModulesFile(final String externalModulesFile) {
    this.externalModulesFile = externalModulesFile;
  }
  
  @Override
  @Pure
  public boolean equals(final Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    ErlangProjectProperties other = (ErlangProjectProperties) obj;
    if (this.outputDir == null) {
      if (other.outputDir != null)
        return false;
    } else if (!this.outputDir.equals(other.outputDir))
      return false;
    if (this.sourceDirs == null) {
      if (other.sourceDirs != null)
        return false;
    } else if (!this.sourceDirs.equals(other.sourceDirs))
      return false;
    if (this.includeDirs == null) {
      if (other.includeDirs != null)
        return false;
    } else if (!this.includeDirs.equals(other.includeDirs))
      return false;
    if (this.testDirs == null) {
      if (other.testDirs != null)
        return false;
    } else if (!this.testDirs.equals(other.testDirs))
      return false;
    if (this.requiredRuntimeVersion == null) {
      if (other.requiredRuntimeVersion != null)
        return false;
    } else if (!this.requiredRuntimeVersion.equals(other.requiredRuntimeVersion))
      return false;
    if (this.externalIncludesFile == null) {
      if (other.externalIncludesFile != null)
        return false;
    } else if (!this.externalIncludesFile.equals(other.externalIncludesFile))
      return false;
    if (this.externalModulesFile == null) {
      if (other.externalModulesFile != null)
        return false;
    } else if (!this.externalModulesFile.equals(other.externalModulesFile))
      return false;
    return true;
  }
  
  @Override
  @Pure
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((this.outputDir== null) ? 0 : this.outputDir.hashCode());
    result = prime * result + ((this.sourceDirs== null) ? 0 : this.sourceDirs.hashCode());
    result = prime * result + ((this.includeDirs== null) ? 0 : this.includeDirs.hashCode());
    result = prime * result + ((this.testDirs== null) ? 0 : this.testDirs.hashCode());
    result = prime * result + ((this.requiredRuntimeVersion== null) ? 0 : this.requiredRuntimeVersion.hashCode());
    result = prime * result + ((this.externalIncludesFile== null) ? 0 : this.externalIncludesFile.hashCode());
    result = prime * result + ((this.externalModulesFile== null) ? 0 : this.externalModulesFile.hashCode());
    return result;
  }
  
  @Override
  @Pure
  public String toString() {
    ToStringBuilder b = new ToStringBuilder(this);
    b.add("outputDir", this.outputDir);
    b.add("sourceDirs", this.sourceDirs);
    b.add("includeDirs", this.includeDirs);
    b.add("testDirs", this.testDirs);
    b.add("requiredRuntimeVersion", this.requiredRuntimeVersion);
    b.add("externalIncludesFile", this.externalIncludesFile);
    b.add("externalModulesFile", this.externalModulesFile);
    return b.toString();
  }
}
