package org.erlide.core.internal.builder.external

import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.Path
import org.erlide.backend.BackendCore
import org.erlide.core.internal.builder.BuildNotifier
import org.erlide.core.internal.builder.ExternalBuilder
import org.erlide.engine.model.builder.BuilderTool
import org.erlide.engine.model.builder.MarkerUtils
import org.erlide.engine.model.root.IErlProject
import org.erlide.util.ErlLogger
import org.erlide.util.SystemConfiguration

class EmakeBuilder extends ExternalBuilder {

    new() {
        super(BuilderTool.EMAKE)
    }

    override getOsCommand(IErlProject erlProject) {
        val backend = BackendCore.backendManager.getBuildBackend(erlProject)
        val path = new Path(backend.runtime.otpHome).append('bin/erl')
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

    override clean(IErlProject erlProject, BuildNotifier notifier) {
        val project = erlProject.workspaceProject
        MarkerUtils.removeProblemMarkersFor(project)
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
