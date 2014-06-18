package org.erlide.engine.model.root

import com.google.common.base.Objects
import com.google.common.collect.Lists
import java.util.Collection
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.erlide.runtime.runtimeinfo.RuntimeVersion
import org.eclipse.core.runtime.preferences.IPreferencesService
import org.erlide.util.PreferencesUtils
import com.google.common.base.Strings
import org.eclipse.core.runtime.Platform

class ErlangProjectProperties {

  @Property IPath outputDir
  @Property Collection<IPath> sourceDirs
  @Property Collection<IPath> includeDirs
  @Property Collection<IPath> testDirs

  @Property RuntimeVersion requiredRuntimeVersion

  @Property String externalIncludesFile
  @Property String externalModulesFile

  val public static ErlangProjectProperties DEFAULT = new ErlangProjectProperties() => [
    _sourceDirs = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS)
    _outputDir = new Path(ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR)
    _includeDirs = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS)
    _testDirs = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_TEST_DIRS)
    _externalIncludesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES
    _externalModulesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES
    _requiredRuntimeVersion = ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION
  ]

  new() {
    _sourceDirs = newArrayList()
    _outputDir = new Path("")
    _includeDirs = newArrayList()
    _testDirs = newArrayList()
    _externalIncludesFile = ""
    _externalModulesFile = ""
    _requiredRuntimeVersion = ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION
  }

  def void setSourceDirs(Collection<IPath> dirs) {
    _sourceDirs = Lists.newArrayList(dirs)
  }

  def void setSourceDirs(IPath... dirs) {
    _sourceDirs = Lists.newArrayList(dirs)
  }

  def void setIncludeDirs(Collection<IPath> dirs) {
    _includeDirs = Lists.newArrayList(dirs)
  }

  def void setIncludeDirs(IPath... dirs) {
    _includeDirs = Lists.newArrayList(dirs)
  }

  def void setTestDirs(Collection<IPath> dirs) {
    _testDirs = Lists.newArrayList(dirs)
  }

  def void setTestDirs(IPath... dirs) {
    _testDirs = Lists.newArrayList(dirs)
  }

  def void copyFrom(ErlangProjectProperties props) {
    _includeDirs = props._includeDirs
    _testDirs = props._testDirs
    _sourceDirs = props._sourceDirs
    _outputDir = props._outputDir
    _requiredRuntimeVersion = props._requiredRuntimeVersion
    _externalIncludesFile = props._externalIncludesFile
    _externalModulesFile = props._externalModulesFile
  }

  def boolean sameAs(Object other1) {
    if (this === other1)
      return true
    if (other1 === null)
      return false
    if (!(other1 instanceof ErlangProjectProperties))
      return false
    val other = other1 as ErlangProjectProperties
    if (_outputDir === null) {
      if (other._outputDir !== null)
        return false
    } else if (!_outputDir.equals(other._outputDir))
      return false
    if (_sourceDirs === null) {
      if (other._sourceDirs !== null)
        return false
    } else if (!_sourceDirs.equals(other._sourceDirs))
      return false
    if (_includeDirs === null) {
      if (other._includeDirs !== null)
        return false
    } else if (!_includeDirs.equals(other._includeDirs))
      return false
    if (_testDirs === null) {
      if (other._testDirs !== null)
        return false
    } else if (!_testDirs.equals(other._testDirs))
      return false
    if (_externalIncludesFile === null) {
      if (other._externalIncludesFile !== null)
        return false
    } else if (!_externalIncludesFile.equals(other._externalIncludesFile))
      return false
    if (_externalModulesFile === null) {
      if (other._externalModulesFile !== null)
        return false
    } else if (!_externalModulesFile.equals(other._externalModulesFile))
      return false
    if (_requiredRuntimeVersion === null) {
      if (other._requiredRuntimeVersion !== null)
        return false
    } else if (!_requiredRuntimeVersion.equals(other._requiredRuntimeVersion))
      return false
    return true
  }

  override toString() {
    val helper = Objects.toStringHelper(this) => [
      add("outputDir", _outputDir)
      add("sources", _sourceDirs)
      add("includes", _includeDirs)
      add("tests", _testDirs)
      add("runtimeVersion", _requiredRuntimeVersion)
    ]
    helper.toString
  }

  def String getExternalIncludes() {
    val externalIncludesString = getExternal(ExternalKind.EXTERNAL_INCLUDES);
    return externalIncludesString;
  }

  def String getExternalModules() {
    val externalModulesString = getExternal(ExternalKind.EXTERNAL_MODULES);
    return externalModulesString;

  }

  def private String getExternal(ExternalKind external) {
    val IPreferencesService service = Platform.getPreferencesService()
    val String key = if (external == ExternalKind.EXTERNAL_INCLUDES) "default_external_includes" else "default_external_modules"
    var String result = getExternal(external, service, key, "org.erlide.ui")
    if (Strings.isNullOrEmpty(result)) {
        // FIXME this is kind of an indirect dep on core plugin (needs to be started)
      result = getExternal(external, service, key, "org.erlide.core")
    }
    return result
  }

  def private String getExternal(ExternalKind external, IPreferencesService service, String key, String pluginId) {
    val String global = service.getString(pluginId, key, "", null)
    val String projprefs = if (external == ExternalKind.EXTERNAL_INCLUDES)
        getExternalIncludesFile()
      else
        getExternalModulesFile()
    return PreferencesUtils.packArray(#[projprefs, global])
  }

}
