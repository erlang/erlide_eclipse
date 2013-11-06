package org.erlide.engine.model.root;

import com.google.common.base.Charsets;
import com.google.common.collect.Lists;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.engine.model.root.PathSerializer;
import org.erlide.engine.model.root.ProjectPreferencesConstants;
import org.erlide.runtime.api.RuntimeCore;
import org.erlide.runtime.runtimeinfo.IRuntimeInfoCatalog;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;

@SuppressWarnings("all")
public class ErlangProjectProperties {
  private IPath _outputDir;
  
  public IPath getOutputDir() {
    return this._outputDir;
  }
  
  public void setOutputDir(final IPath outputDir) {
    this._outputDir = outputDir;
  }
  
  private Collection<IPath> sourceDirs;
  
  private Collection<IPath> includeDirs;
  
  private String _externalIncludesFile;
  
  public String getExternalIncludesFile() {
    return this._externalIncludesFile;
  }
  
  public void setExternalIncludesFile(final String externalIncludesFile) {
    this._externalIncludesFile = externalIncludesFile;
  }
  
  private String _externalModulesFile;
  
  public String getExternalModulesFile() {
    return this._externalModulesFile;
  }
  
  public void setExternalModulesFile(final String externalModulesFile) {
    this._externalModulesFile = externalModulesFile;
  }
  
  private RuntimeVersion runtimeVersion;
  
  private String runtimeName;
  
  private boolean _nukeOutputOnClean;
  
  public boolean isNukeOutputOnClean() {
    return this._nukeOutputOnClean;
  }
  
  public void setNukeOutputOnClean(final boolean nukeOutputOnClean) {
    this._nukeOutputOnClean = nukeOutputOnClean;
  }
  
  private Charset _encoding;
  
  public Charset getEncoding() {
    return this._encoding;
  }
  
  public void setEncoding(final Charset encoding) {
    this._encoding = encoding;
  }
  
  public ErlangProjectProperties() {
    Collection<IPath> _unpackList = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS);
    this.sourceDirs = _unpackList;
    Path _path = new Path(ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR);
    this.setOutputDir(_path);
    Collection<IPath> _unpackList_1 = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS);
    this.includeDirs = _unpackList_1;
    this.setExternalIncludesFile(ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES);
    this.setExternalModulesFile(ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES);
    RuntimeVersion _runtimeVersion = new RuntimeVersion(ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION);
    this.runtimeVersion = _runtimeVersion;
    this.runtimeName = null;
    this.setNukeOutputOnClean(false);
    RuntimeVersion _runtimeVersion_1 = new RuntimeVersion(18);
    boolean _isCompatible = this.runtimeVersion.isCompatible(_runtimeVersion_1);
    if (_isCompatible) {
      this.setEncoding(Charsets.UTF_8);
    } else {
      this.setEncoding(Charsets.ISO_8859_1);
    }
  }
  
  public Collection<IPath> getIncludeDirs() {
    Collection<IPath> _unmodifiableCollection = Collections.<IPath>unmodifiableCollection(this.includeDirs);
    return _unmodifiableCollection;
  }
  
  public Collection<IPath> setIncludeDirs(final Collection<IPath> includeDirs2) {
    ArrayList<IPath> _newArrayList = Lists.<IPath>newArrayList(includeDirs2);
    Collection<IPath> _includeDirs = this.includeDirs = _newArrayList;
    return _includeDirs;
  }
  
  public Collection<IPath> setIncludeDirs(final IPath... includeDirs2) {
    ArrayList<IPath> _newArrayList = Lists.<IPath>newArrayList(includeDirs2);
    Collection<IPath> _includeDirs = this.includeDirs = _newArrayList;
    return _includeDirs;
  }
  
  public Collection<IPath> getSourceDirs() {
    Collection<IPath> _unmodifiableCollection = Collections.<IPath>unmodifiableCollection(this.sourceDirs);
    return _unmodifiableCollection;
  }
  
  public Collection<IPath> setSourceDirs(final Collection<IPath> sourceDirs2) {
    ArrayList<IPath> _newArrayList = Lists.<IPath>newArrayList(sourceDirs2);
    Collection<IPath> _sourceDirs = this.sourceDirs = _newArrayList;
    return _sourceDirs;
  }
  
  public Collection<IPath> setSourceDirs(final IPath... sourceDirs2) {
    ArrayList<IPath> _newArrayList = Lists.<IPath>newArrayList(sourceDirs2);
    Collection<IPath> _sourceDirs = this.sourceDirs = _newArrayList;
    return _sourceDirs;
  }
  
  public RuntimeVersion copyFrom(final ErlangProjectProperties erlangProjectProperties) {
    RuntimeVersion _xblockexpression = null;
    {
      final ErlangProjectProperties bprefs = erlangProjectProperties;
      this.includeDirs = bprefs.includeDirs;
      this.sourceDirs = bprefs.sourceDirs;
      IPath _outputDir = bprefs.getOutputDir();
      this.setOutputDir(_outputDir);
      this.runtimeName = bprefs.runtimeName;
      RuntimeVersion _requiredRuntimeVersion = bprefs.getRequiredRuntimeVersion();
      RuntimeVersion _runtimeVersion = this.runtimeVersion = _requiredRuntimeVersion;
      _xblockexpression = (_runtimeVersion);
    }
    return _xblockexpression;
  }
  
  public RuntimeInfo getRuntimeInfo() {
    RuntimeInfo _xblockexpression = null;
    {
      IRuntimeInfoCatalog _runtimeInfoCatalog = RuntimeCore.getRuntimeInfoCatalog();
      final RuntimeInfo runtime = _runtimeInfoCatalog.getRuntime(this.runtimeVersion, this.runtimeName);
      _xblockexpression = (runtime);
    }
    return _xblockexpression;
  }
  
  public RuntimeVersion getRuntimeVersion() {
    RuntimeVersion _xblockexpression = null;
    {
      final RuntimeInfo runtimeInfo = this.getRuntimeInfo();
      RuntimeVersion _xifexpression = null;
      boolean _tripleNotEquals = (runtimeInfo != null);
      if (_tripleNotEquals) {
        RuntimeVersion _version = runtimeInfo.getVersion();
        _xifexpression = _version;
      } else {
        _xifexpression = this.runtimeVersion;
      }
      _xblockexpression = (_xifexpression);
    }
    return _xblockexpression;
  }
  
  public void setRuntimeVersion(final RuntimeVersion runtimeVersion) {
    this.runtimeVersion = runtimeVersion;
    RuntimeVersion _runtimeVersion = new RuntimeVersion(18);
    boolean _isCompatible = runtimeVersion.isCompatible(_runtimeVersion);
    if (_isCompatible) {
      this.setEncoding(Charsets.UTF_8);
    } else {
      this.setEncoding(Charsets.ISO_8859_1);
    }
  }
  
  public RuntimeVersion getRequiredRuntimeVersion() {
    return this.runtimeVersion;
  }
  
  @Deprecated
  public String getRuntimeName() {
    return this.runtimeName;
  }
  
  @Deprecated
  public String setRuntimeName(final String runtimeName) {
    String _runtimeName = this.runtimeName = runtimeName;
    return _runtimeName;
  }
}
