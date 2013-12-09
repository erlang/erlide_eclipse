package org.erlide.engine.model.root;

import com.google.common.base.Objects;
import com.google.common.base.Objects.ToStringHelper;
import com.google.common.collect.Lists;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import org.eclipse.core.resources.IPathVariableManager;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.eclipse.xtext.xbase.lib.ObjectExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.engine.model.root.PathSerializer;
import org.erlide.engine.model.root.ProjectPreferencesConstants;
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
  
  private Collection<IPath> _sourceDirs;
  
  public Collection<IPath> getSourceDirs() {
    return this._sourceDirs;
  }
  
  private Collection<IPath> _includeDirs;
  
  public Collection<IPath> getIncludeDirs() {
    return this._includeDirs;
  }
  
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
  
  private RuntimeVersion _requiredRuntimeVersion;
  
  public RuntimeVersion getRequiredRuntimeVersion() {
    return this._requiredRuntimeVersion;
  }
  
  public void setRequiredRuntimeVersion(final RuntimeVersion requiredRuntimeVersion) {
    this._requiredRuntimeVersion = requiredRuntimeVersion;
  }
  
  public final static ErlangProjectProperties DEFAULT = new Function0<ErlangProjectProperties>() {
    public ErlangProjectProperties apply() {
      ErlangProjectProperties _erlangProjectProperties = new ErlangProjectProperties();
      final Procedure1<ErlangProjectProperties> _function = new Procedure1<ErlangProjectProperties>() {
        public void apply(final ErlangProjectProperties it) {
          Collection<IPath> _unpackList = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS);
          it._sourceDirs = _unpackList;
          Path _path = new Path(ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR);
          it._outputDir = _path;
          Collection<IPath> _unpackList_1 = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS);
          it._includeDirs = _unpackList_1;
          it._externalIncludesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES;
          it._externalModulesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES;
          RuntimeVersion _runtimeVersion = new RuntimeVersion(ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION);
          it._requiredRuntimeVersion = _runtimeVersion;
        }
      };
      ErlangProjectProperties _doubleArrow = ObjectExtensions.<ErlangProjectProperties>operator_doubleArrow(_erlangProjectProperties, _function);
      return _doubleArrow;
    }
  }.apply();
  
  public ErlangProjectProperties() {
    ArrayList<IPath> _newArrayList = CollectionLiterals.<IPath>newArrayList();
    this._sourceDirs = _newArrayList;
    Path _path = new Path("");
    this._outputDir = _path;
    ArrayList<IPath> _newArrayList_1 = CollectionLiterals.<IPath>newArrayList();
    this._includeDirs = _newArrayList_1;
    this._externalIncludesFile = "";
    this._externalModulesFile = "";
    RuntimeVersion _runtimeVersion = new RuntimeVersion(ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION);
    this._requiredRuntimeVersion = _runtimeVersion;
  }
  
  public void setIncludeDirs(final Collection<IPath> dirs) {
    ArrayList<IPath> _newArrayList = Lists.<IPath>newArrayList(dirs);
    this._includeDirs = _newArrayList;
  }
  
  public void setIncludeDirs(final IPath... dirs) {
    ArrayList<IPath> _newArrayList = Lists.<IPath>newArrayList(dirs);
    this._includeDirs = _newArrayList;
  }
  
  public void setSourceDirs(final Collection<IPath> dirs) {
    ArrayList<IPath> _newArrayList = Lists.<IPath>newArrayList(dirs);
    this._sourceDirs = _newArrayList;
  }
  
  public void setSourceDirs(final IPath... dirs) {
    ArrayList<IPath> _newArrayList = Lists.<IPath>newArrayList(dirs);
    this._sourceDirs = _newArrayList;
  }
  
  public void copyFrom(final ErlangProjectProperties props) {
    this._includeDirs = props._includeDirs;
    this._sourceDirs = props._sourceDirs;
    this._outputDir = props._outputDir;
    this._requiredRuntimeVersion = props._requiredRuntimeVersion;
    this._externalIncludesFile = props._externalIncludesFile;
    this._externalModulesFile = props._externalModulesFile;
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
    boolean _tripleEquals_2 = (this._outputDir == null);
    if (_tripleEquals_2) {
      boolean _tripleNotEquals = (other._outputDir != null);
      if (_tripleNotEquals) {
        return false;
      }
    } else {
      boolean _equals = this._outputDir.equals(other._outputDir);
      boolean _not_1 = (!_equals);
      if (_not_1) {
        return false;
      }
    }
    boolean _tripleEquals_3 = (this._sourceDirs == null);
    if (_tripleEquals_3) {
      boolean _tripleNotEquals_1 = (other._sourceDirs != null);
      if (_tripleNotEquals_1) {
        return false;
      }
    } else {
      boolean _equals_1 = this._sourceDirs.equals(other._sourceDirs);
      boolean _not_2 = (!_equals_1);
      if (_not_2) {
        return false;
      }
    }
    boolean _tripleEquals_4 = (this._includeDirs == null);
    if (_tripleEquals_4) {
      boolean _tripleNotEquals_2 = (other._includeDirs != null);
      if (_tripleNotEquals_2) {
        return false;
      }
    } else {
      boolean _equals_2 = this._includeDirs.equals(other._includeDirs);
      boolean _not_3 = (!_equals_2);
      if (_not_3) {
        return false;
      }
    }
    boolean _tripleEquals_5 = (this._externalIncludesFile == null);
    if (_tripleEquals_5) {
      boolean _tripleNotEquals_3 = (other._externalIncludesFile != null);
      if (_tripleNotEquals_3) {
        return false;
      }
    } else {
      boolean _equals_3 = this._externalIncludesFile.equals(other._externalIncludesFile);
      boolean _not_4 = (!_equals_3);
      if (_not_4) {
        return false;
      }
    }
    boolean _tripleEquals_6 = (this._externalModulesFile == null);
    if (_tripleEquals_6) {
      boolean _tripleNotEquals_4 = (other._externalModulesFile != null);
      if (_tripleNotEquals_4) {
        return false;
      }
    } else {
      boolean _equals_4 = this._externalModulesFile.equals(other._externalModulesFile);
      boolean _not_5 = (!_equals_4);
      if (_not_5) {
        return false;
      }
    }
    boolean _tripleEquals_7 = (this._requiredRuntimeVersion == null);
    if (_tripleEquals_7) {
      boolean _tripleNotEquals_5 = (other._requiredRuntimeVersion != null);
      if (_tripleNotEquals_5) {
        return false;
      }
    } else {
      boolean _equals_5 = this._requiredRuntimeVersion.equals(other._requiredRuntimeVersion);
      boolean _not_6 = (!_equals_5);
      if (_not_6) {
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
          it.add("outputDir", ErlangProjectProperties.this._outputDir);
          it.add("sources", ErlangProjectProperties.this._sourceDirs);
          it.add("includes", ErlangProjectProperties.this._includeDirs);
          it.add("runtimeVersion", ErlangProjectProperties.this._requiredRuntimeVersion);
        }
      };
      final ToStringHelper helper = ObjectExtensions.<ToStringHelper>operator_doubleArrow(_stringHelper, _function);
      String _string = helper.toString();
      _xblockexpression = (_string);
    }
    return _xblockexpression;
  }
  
  public ErlangProjectProperties resolve() {
    ErlangProjectProperties _erlangProjectProperties = new ErlangProjectProperties();
    final ErlangProjectProperties result = _erlangProjectProperties;
    result.copyFrom(this);
    final ErlangProjectProperties dflt = ErlangProjectProperties.DEFAULT;
    IPath _outputDir = result.getOutputDir();
    boolean _equals = Objects.equal(_outputDir, null);
    if (_equals) {
      IPath _outputDir_1 = dflt.getOutputDir();
      result.setOutputDir(_outputDir_1);
    }
    IPath _outputDir_2 = result.getOutputDir();
    IPath _resolvePath = this.resolvePath(_outputDir_2);
    result.setOutputDir(_resolvePath);
    Collection<IPath> _sourceDirs = result.getSourceDirs();
    boolean _equals_1 = Objects.equal(_sourceDirs, null);
    if (_equals_1) {
      Collection<IPath> _sourceDirs_1 = dflt.getSourceDirs();
      result.setSourceDirs(_sourceDirs_1);
    }
    Collection<IPath> _sourceDirs_2 = result.getSourceDirs();
    Collection<IPath> _resolvePaths = this.resolvePaths(_sourceDirs_2);
    result.setSourceDirs(_resolvePaths);
    Collection<IPath> _includeDirs = result.getIncludeDirs();
    boolean _equals_2 = Objects.equal(_includeDirs, null);
    if (_equals_2) {
      Collection<IPath> _includeDirs_1 = dflt.getIncludeDirs();
      result.setIncludeDirs(_includeDirs_1);
    }
    Collection<IPath> _includeDirs_2 = result.getIncludeDirs();
    Collection<IPath> _resolvePaths_1 = this.resolvePaths(_includeDirs_2);
    result.setIncludeDirs(_resolvePaths_1);
    return result;
  }
  
  private Collection<IPath> resolvePaths(final Collection<IPath> paths) {
    IWorkspace _workspace = ResourcesPlugin.getWorkspace();
    final IPathVariableManager pathVariableManager = _workspace.getPathVariableManager();
    int _size = paths.size();
    final List<IPath> result = Lists.<IPath>newArrayListWithCapacity(_size);
    for (final IPath path : paths) {
      {
        final IPath resolvedPath = pathVariableManager.resolvePath(path);
        result.add(resolvedPath);
      }
    }
    return Collections.<IPath>unmodifiableCollection(result);
  }
  
  private IPath resolvePath(final IPath path) {
    IWorkspace _workspace = ResourcesPlugin.getWorkspace();
    final IPathVariableManager pathVariableManager = _workspace.getPathVariableManager();
    return pathVariableManager.resolvePath(path);
  }
}
