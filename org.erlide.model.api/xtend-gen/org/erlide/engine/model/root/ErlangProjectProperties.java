package org.erlide.engine.model.root;

import com.google.common.base.Charsets;
import com.google.common.base.Objects;
import com.google.common.base.Objects.ToStringHelper;
import com.google.common.collect.Lists;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
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
  
  public boolean sameAs(final Object other1) {
    boolean _tripleEquals = (this == other1);
    if (_tripleEquals) {
      return true;
    }
    boolean _tripleEquals_1 = (other1 == null);
    if (_tripleEquals_1) {
      return false;
    }
    boolean _not = (!(other1 instanceof ErlangProjectProperties));
    if (_not) {
      return false;
    }
    final ErlangProjectProperties other = ((ErlangProjectProperties) other1);
    IPath _outputDir = this.getOutputDir();
    boolean _tripleEquals_2 = (_outputDir == null);
    if (_tripleEquals_2) {
      IPath _outputDir_1 = other.getOutputDir();
      boolean _tripleNotEquals = (_outputDir_1 != null);
      if (_tripleNotEquals) {
        return false;
      }
    } else {
      IPath _outputDir_2 = this.getOutputDir();
      IPath _outputDir_3 = other.getOutputDir();
      boolean _equals = _outputDir_2.equals(_outputDir_3);
      boolean _not_1 = (!_equals);
      if (_not_1) {
        return false;
      }
    }
    boolean _tripleEquals_3 = (this.sourceDirs == null);
    if (_tripleEquals_3) {
      boolean _tripleNotEquals_1 = (other.sourceDirs != null);
      if (_tripleNotEquals_1) {
        return false;
      }
    } else {
      boolean _equals_1 = this.sourceDirs.equals(other.sourceDirs);
      boolean _not_2 = (!_equals_1);
      if (_not_2) {
        return false;
      }
    }
    boolean _tripleEquals_4 = (this.includeDirs == null);
    if (_tripleEquals_4) {
      boolean _tripleNotEquals_2 = (other.includeDirs != null);
      if (_tripleNotEquals_2) {
        return false;
      }
    } else {
      boolean _equals_2 = this.includeDirs.equals(other.includeDirs);
      boolean _not_3 = (!_equals_2);
      if (_not_3) {
        return false;
      }
    }
    String _externalIncludesFile = this.getExternalIncludesFile();
    boolean _tripleEquals_5 = (_externalIncludesFile == null);
    if (_tripleEquals_5) {
      String _externalIncludesFile_1 = other.getExternalIncludesFile();
      boolean _tripleNotEquals_3 = (_externalIncludesFile_1 != null);
      if (_tripleNotEquals_3) {
        return false;
      }
    } else {
      String _externalIncludesFile_2 = this.getExternalIncludesFile();
      String _externalIncludesFile_3 = other.getExternalIncludesFile();
      boolean _equals_3 = _externalIncludesFile_2.equals(_externalIncludesFile_3);
      boolean _not_4 = (!_equals_3);
      if (_not_4) {
        return false;
      }
    }
    boolean _tripleEquals_6 = (this._externalModulesFile == null);
    if (_tripleEquals_6) {
      String _externalModulesFile = other.getExternalModulesFile();
      boolean _tripleNotEquals_4 = (_externalModulesFile != null);
      if (_tripleNotEquals_4) {
        return false;
      }
    } else {
      String _externalModulesFile_1 = this.getExternalModulesFile();
      String _externalModulesFile_2 = other.getExternalModulesFile();
      boolean _equals_4 = _externalModulesFile_1.equals(_externalModulesFile_2);
      boolean _not_5 = (!_equals_4);
      if (_not_5) {
        return false;
      }
    }
    boolean _tripleEquals_7 = (this.runtimeVersion == null);
    if (_tripleEquals_7) {
      boolean _tripleNotEquals_5 = (other.runtimeVersion != null);
      if (_tripleNotEquals_5) {
        return false;
      }
    } else {
      boolean _equals_5 = this.runtimeVersion.equals(other.runtimeVersion);
      boolean _not_6 = (!_equals_5);
      if (_not_6) {
        return false;
      }
    }
    boolean _tripleEquals_8 = (this.runtimeName == null);
    if (_tripleEquals_8) {
      boolean _tripleNotEquals_6 = (other.runtimeName != null);
      if (_tripleNotEquals_6) {
        return false;
      }
    } else {
      boolean _equals_6 = this.runtimeName.equals(other.runtimeName);
      boolean _not_7 = (!_equals_6);
      if (_not_7) {
        return false;
      }
    }
    boolean _isNukeOutputOnClean = other.isNukeOutputOnClean();
    boolean _isNukeOutputOnClean_1 = this.isNukeOutputOnClean();
    boolean _notEquals = (_isNukeOutputOnClean != _isNukeOutputOnClean_1);
    if (_notEquals) {
      return false;
    }
    Charset _encoding = this.getEncoding();
    boolean _tripleEquals_9 = (_encoding == null);
    if (_tripleEquals_9) {
      Charset _encoding_1 = other.getEncoding();
      boolean _tripleNotEquals_7 = (_encoding_1 != null);
      if (_tripleNotEquals_7) {
        return false;
      }
    } else {
      Charset _encoding_2 = this.getEncoding();
      Charset _encoding_3 = other.getEncoding();
      boolean _equals_7 = _encoding_2.equals(_encoding_3);
      boolean _not_8 = (!_equals_7);
      if (_not_8) {
        return false;
      }
    }
    return true;
  }
  
  public String toString() {
    String _xblockexpression = null;
    {
      ToStringHelper _stringHelper = Objects.toStringHelper(this);
      final Procedure1<ToStringHelper> _function = new Procedure1<ToStringHelper>() {
        public void apply(final ToStringHelper it) {
          IPath _outputDir = ErlangProjectProperties.this.getOutputDir();
          it.add("outputDir", _outputDir);
          it.add("sources", ErlangProjectProperties.this.sourceDirs);
        }
      };
      final ToStringHelper helper = ObjectExtensions.<ToStringHelper>operator_doubleArrow(_stringHelper, _function);
      String _string = helper.toString();
      _xblockexpression = (_string);
    }
    return _xblockexpression;
  }
}
