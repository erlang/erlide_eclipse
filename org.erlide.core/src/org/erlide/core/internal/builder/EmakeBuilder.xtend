package org.erlide.core.internal.builder

import java.io.StringBufferInputStream
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.Path
import org.erlide.backend.BackendCore
import org.erlide.core.ErlangCore
import org.erlide.core.builder.MarkerUtils
import org.erlide.engine.ErlangEngine
import org.erlide.engine.model.root.IErlangProjectProperties
import org.erlide.util.ErlLogger

@SuppressWarnings('deprecation')
class EmakeBuilder extends ExternalBuilder {
    override getOsCommand() {
        val backend = BackendCore::backendManager.getBuildBackend(project)
        val path = new Path(backend.runtimeInfo.otpHome).append('bin/erl')
        path.toOSString
    }

    protected override getCompileTarget() {
        '-make'
    }

    protected override getCleanTarget() {
        null
    }

    override getConfigParser() {
        null
    }

    override clean(IProgressMonitor monitor) {
        val project = project
        MarkerUtils::removeProblemMarkersFor(project)
        val erlProject = ErlangEngine::instance.model.getErlangProject(project)
        val bf = project.getFolder(erlProject.outputLocation)
        if (bf.exists) {
            try {
                for (f : bf.members) {
                    try {
                        f.delete(true, null)
                    } catch (CoreException e) {
                        ErlLogger::warn('Could not clean up output directory ' + bf.location)
                    }
                }
            } catch (CoreException e) {
            }
        }
    }

    override createConfig(IErlangProjectProperties info) {
        val config = project.getFile('Emakefile')
        try {
            val template = '''
                «FOR src : info.sourceDirs»
                    {'«src.toPortableString»/*',[«FOR inc : info.includeDirs»{i, "«inc.toPortableString»"},«ENDFOR»]}.
                «ENDFOR»
            '''
            println(">> " + template)
            config.create(new StringBufferInputStream(template), true, null)
        } catch (CoreException e) {
            ErlLogger::error(e)
        }
    }

    override getId() {
        ErlangCore::PLUGIN_ID + '.emake.builder'
    }
}
