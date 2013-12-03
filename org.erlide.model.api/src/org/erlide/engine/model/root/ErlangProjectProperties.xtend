package org.erlide.engine.model.root

import com.google.common.base.Charsets
import com.google.common.collect.Lists
import java.util.Collection
import java.util.Collections
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.erlide.runtime.api.RuntimeCore
import org.erlide.runtime.runtimeinfo.RuntimeVersion
import java.nio.charset.Charset
import com.google.common.base.Objects
import org.eclipse.core.resources.ResourcesPlugin
import java.util.List

class ErlangProjectProperties {
    @Property IPath outputDir
    @Property Collection<IPath> sourceDirs
    @Property Collection<IPath> includeDirs
    @Property String externalIncludesFile
    @Property String externalModulesFile
    @Property RuntimeVersion requiredRuntimeVersion
    @Property boolean nukeOutputOnClean
    @Property Charset encoding
    @Property Object builderData

    val public static ErlangProjectProperties DEFAULT = new ErlangProjectProperties() => [
        _sourceDirs = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS)
        _outputDir = new Path(ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR)
        _includeDirs = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS)
        _externalIncludesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES
        _externalModulesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES
        _requiredRuntimeVersion = new RuntimeVersion(ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION)
        _nukeOutputOnClean = false
        if (_requiredRuntimeVersion.isCompatible(new RuntimeVersion(18))) {
            _encoding = Charsets.UTF_8
        } else {
            _encoding = Charsets.ISO_8859_1
        }
    ]

    new() {
        _sourceDirs = newArrayList()
        _outputDir = new Path("")
        _includeDirs = newArrayList()
        _externalIncludesFile = ""
        _externalModulesFile = ""
        _requiredRuntimeVersion = new RuntimeVersion(ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION)
        _nukeOutputOnClean = false
        if (_requiredRuntimeVersion.isCompatible(new RuntimeVersion(18))) {
            _encoding = Charsets.UTF_8
        } else {
            _encoding = Charsets.ISO_8859_1
        }
    }

    def getIncludeDirs() {
        Collections.unmodifiableCollection(_includeDirs)
    }

    def void setIncludeDirs(Collection<IPath> includeDirs2) {
        _includeDirs = Lists.newArrayList(includeDirs2)
    }

    def void setIncludeDirs(IPath... includeDirs2) {
        _includeDirs = Lists.newArrayList(includeDirs2)
    }

    def getSourceDirs() {
        Collections.unmodifiableCollection(_sourceDirs)
    }

    def void setSourceDirs(Collection<IPath> sourceDirs2) {
        _sourceDirs = Lists.newArrayList(sourceDirs2)
    }

    def void setSourceDirs(IPath... sourceDirs2) {
        _sourceDirs = Lists.newArrayList(sourceDirs2)
    }

    def void copyFrom(ErlangProjectProperties props) {
        _includeDirs = props._includeDirs
        _sourceDirs = props._sourceDirs
        _outputDir = props._outputDir
        _requiredRuntimeVersion = props._requiredRuntimeVersion
        _encoding = props._encoding
        _externalIncludesFile = props._externalIncludesFile
        _externalModulesFile = props._externalModulesFile
        _nukeOutputOnClean = props._nukeOutputOnClean
        _builderData = props._builderData
    }

    def getRuntimeInfo() {
        val runtime = RuntimeCore.runtimeInfoCatalog.getRuntime(_requiredRuntimeVersion, null)
        runtime
    }

    def getRuntimeVersion() {

        // XXX ???
        val runtimeInfo = runtimeInfo
        if (runtimeInfo !== null) {
            runtimeInfo.version
        } else {
            _requiredRuntimeVersion
        }
    }

    def setRuntimeVersion(RuntimeVersion runtimeVersion) {
        this._requiredRuntimeVersion = runtimeVersion
        if (_requiredRuntimeVersion.isCompatible(new RuntimeVersion(18))) {
            _encoding = Charsets.UTF_8
        } else {
            _encoding = Charsets.ISO_8859_1
        }
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
        if (other._nukeOutputOnClean != _nukeOutputOnClean)
            return false
        if (_encoding === null) {
            if (other._encoding !== null)
                return false
        } else if (!_encoding.equals(other._encoding))
            return false
        return true
    }

    override toString() {
        val helper = Objects.toStringHelper(this) => [
            add("outputDir", _outputDir)
            add("sources", _sourceDirs)
            add("includes", _includeDirs)
            add("runtimeVersion", _requiredRuntimeVersion)
            add("encoding", _encoding)
        ]
        helper.toString
    }

    def ErlangProjectProperties resolve() {
        val result = new ErlangProjectProperties()
        result.copyFrom(this)
        val dflt = ErlangProjectProperties.DEFAULT
        if (result.getOutputDir() == null) {
            result.setOutputDir(dflt.getOutputDir())
        }
        result.setOutputDir(resolvePath(result.getOutputDir()))
        if (result.getSourceDirs() == null) {
            result.setSourceDirs(dflt.getSourceDirs())
        }
        result.setSourceDirs(resolvePaths(result.getSourceDirs()))
        if (result.getIncludeDirs() == null) {
            result.setIncludeDirs(dflt.getIncludeDirs())
        }
        result.setIncludeDirs(resolvePaths(result.getIncludeDirs()))
        return result;
    }

    def private Collection<IPath> resolvePaths(Collection<IPath> paths) {
        val pathVariableManager = ResourcesPlugin.getWorkspace().getPathVariableManager()
        val List<IPath> result = Lists.newArrayListWithCapacity(paths.size())
        for (IPath path : paths) {
            val resolvedPath = pathVariableManager.resolvePath(path)
            result.add(resolvedPath)
        }
        return Collections.unmodifiableCollection(result)
    }

    def private IPath resolvePath(IPath path) {
        val pathVariableManager = ResourcesPlugin.getWorkspace().getPathVariableManager()
        return pathVariableManager.resolvePath(path)
    }

}
