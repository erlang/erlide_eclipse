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
        if (runtimeVersion.isCompatible(new RuntimeVersion(18))) {
            encoding = Charsets.UTF_8
        } else {
            encoding = Charsets.ISO_8859_1
        }
    }

    def getIncludeDirs() {
        Collections.unmodifiableCollection(includeDirs)
    }

    def setIncludeDirs(Collection<IPath> includeDirs2) {
        includeDirs = Lists.newArrayList(includeDirs2)
    }

    def setIncludeDirs(IPath... includeDirs2) {
        includeDirs = Lists.newArrayList(includeDirs2)
    }

    def getSourceDirs() {
        Collections.unmodifiableCollection(sourceDirs)
    }

    def setSourceDirs(Collection<IPath> sourceDirs2) {
        sourceDirs = Lists.newArrayList(sourceDirs2)
    }

    def setSourceDirs(IPath... sourceDirs2) {
        sourceDirs = Lists.newArrayList(sourceDirs2)
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
        val runtime = RuntimeCore.runtimeInfoCatalog.getRuntime(runtimeVersion, runtimeName)
        runtime
    }

    def getRuntimeVersion() {

        // XXX ???
        val runtimeInfo = runtimeInfo
        if (runtimeInfo !== null) {
            runtimeInfo.version
        } else {
            runtimeVersion
        }
    }

    def setRuntimeVersion(RuntimeVersion runtimeVersion) {
        this.runtimeVersion = runtimeVersion
        if (runtimeVersion.isCompatible(new RuntimeVersion(18))) {
            encoding = Charsets.UTF_8
        } else {
            encoding = Charsets.ISO_8859_1
        }
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

    def boolean sameAs(Object other1) {
        if (this === other1)
            return true
        if (other1 === null)
            return false
        if (!(other1 instanceof ErlangProjectProperties))
            return false
        val other = other1 as ErlangProjectProperties
        if (outputDir === null) {
            if (other.outputDir !== null)
                return false
        } else if (!outputDir.equals(other.outputDir))
            return false
        if (sourceDirs === null) {
            if (other.sourceDirs !== null)
                return false
        } else if (!sourceDirs.equals(other.sourceDirs))
            return false
        if (includeDirs === null) {
            if (other.includeDirs !== null)
                return false
        } else if (!includeDirs.equals(other.includeDirs))
            return false
        if (externalIncludesFile === null) {
            if (other.externalIncludesFile !== null)
                return false
        } else if (!externalIncludesFile.equals(other.externalIncludesFile))
            return false
        if (_externalModulesFile === null) {
            if (other.externalModulesFile !== null)
                return false
        } else if (!externalModulesFile.equals(other.externalModulesFile))
            return false
        if (runtimeVersion === null) {
            if (other.runtimeVersion !== null)
                return false
        } else if (!runtimeVersion.equals(other.runtimeVersion))
            return false
        if (runtimeName === null) {
            if (other.runtimeName !== null)
                return false
        } else if (!runtimeName.equals(other.runtimeName))
            return false
        if (other.nukeOutputOnClean != nukeOutputOnClean)
            return false
        if (encoding === null) {
            if (other.encoding !== null)
                return false
        } else if (!encoding.equals(other.encoding))
            return false
        return true
    }

    override toString() {
        val helper = Objects.toStringHelper(this) =>[
            add("outputDir", outputDir)
            add("sources", sourceDirs)
        ]
        helper.toString
    }

}
