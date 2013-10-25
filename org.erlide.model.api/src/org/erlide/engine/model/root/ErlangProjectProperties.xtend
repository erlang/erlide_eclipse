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

class ErlangProjectProperties {
    @Property var IPath outputDir
    var Collection<IPath> sourceDirs
    var Collection<IPath> includeDirs
    @Property var String externalIncludesFile
    @Property var String externalModulesFile
    var RuntimeVersion runtimeVersion
    var String runtimeName
    @Property var boolean nukeOutputOnClean
    @Property var Charset encoding

    new() {
        sourceDirs = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS)
        outputDir = new Path(ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR)
        includeDirs = PathSerializer.unpackList(ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS)
        externalIncludesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES
        externalModulesFile = ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES
        runtimeVersion = new RuntimeVersion(ProjectPreferencesConstants.DEFAULT_RUNTIME_VERSION)
        runtimeName = null
        nukeOutputOnClean = false
        encoding = Charsets.ISO_8859_1
    }

    def getIncludeDirs() {
        Collections::unmodifiableCollection(includeDirs)
    }

    def setIncludeDirs(Collection<IPath> includeDirs2) {
        includeDirs = Lists::newArrayList(includeDirs2)
    }

    def getSourceDirs() {
        Collections::unmodifiableCollection(sourceDirs)
    }

    def setSourceDirs(Collection<IPath> sourceDirs2) {
        sourceDirs = Lists::newArrayList(sourceDirs2)
    }

    def copyFrom(ErlangProjectProperties erlangProjectProperties) {
        val bprefs = erlangProjectProperties
        includeDirs = bprefs.includeDirs
        sourceDirs = bprefs.sourceDirs
        outputDir = bprefs.outputDir
        runtimeName = bprefs.runtimeName
        runtimeVersion = bprefs.requiredRuntimeVersion
    }

    def getRuntimeInfo() {
        val runtime = RuntimeCore::runtimeInfoCatalog.getRuntime(runtimeVersion, runtimeName)
        runtime
    }

    def getRuntimeVersion() {
        val runtimeInfo = runtimeInfo
        if(runtimeInfo !== null) runtimeInfo.version else runtimeVersion
    }

    def setRuntimeVersion(RuntimeVersion runtimeVersion) {
        this.runtimeVersion = runtimeVersion
    }

    def getRequiredRuntimeVersion() {
        runtimeVersion
    }

    @Deprecated def getRuntimeName() {
        runtimeName
    }

    @Deprecated def setRuntimeName(String runtimeName) {
        this.runtimeName = runtimeName
    }

}
