package org.erlide.core.internal.builder.external

import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.Path
import org.erlide.backend.BackendCore
import org.erlide.core.internal.builder.ExternalBuilder
import org.erlide.engine.ErlangEngine
import org.erlide.engine.model.builder.BuilderTool
import org.erlide.engine.model.builder.MarkerUtils
import org.erlide.util.ErlLogger
import org.erlide.util.SystemConfiguration

class EmakeBuilder extends ExternalBuilder {

    new() {
        super(BuilderTool.EMAKE)
    }

    override getOsCommand() {
        val eproject = ErlangEngine.getInstance.model.findProject(project)
        val backend = BackendCore.backendManager.getBuildBackend(eproject)
        val path = new Path(backend.runtimeInfo.otpHome).append('bin/erl')
        if (SystemConfiguration.instance.onWindows)
            path.toPortableString + ".exe"
        else
            path.toPortableString
    }

    protected override getCompileTarget() {
        '-make'
    }

    protected override getCleanTarget() {
        null
    }

    override clean(IProgressMonitor monitor) {
        val project = project
        MarkerUtils.removeProblemMarkersFor(project)
        val erlProject = ErlangEngine.instance.model.getErlangProject(project)
        val bf = project.getFolder(erlProject.properties.outputDir)
        bf => [
            if (exists) {
                members.forEach [
                    try {
                        delete(true, null)
                    } catch (CoreException e) {
                        ErlLogger.warn('Could not clean up output directory ' + location)
                    }
                ]
            }
        ]
    }

    override getProperties() {
        null
    }

}
