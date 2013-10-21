package org.erlide.core.internal.builder.external

import java.util.Map
import org.eclipse.core.resources.IMarker
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.IProgressMonitor
import org.erlide.core.ErlangCore
import org.erlide.core.builder.MarkerUtils
import org.erlide.core.internal.builder.ExternalBuilder
import org.erlide.engine.ErlangEngine
import org.erlide.engine.model.root.IErlFolder

class RebarBuilder extends ExternalBuilder {
    override getOsCommand() {
        'rebar'
    }

    override build(int kind, Map<String, String> args, IProgressMonitor monitor) throws CoreException {
        val result = super.build(kind, args, monitor)
        checkIfProjectHasAppFile
        result
    }

    var boolean foundAppSrc

    private def checkIfProjectHasAppFile() throws CoreException {
        foundAppSrc = false
        project.accept [ resource |
            if (resource.name.endsWith('.app.src')) {
                val folder = (ErlangEngine::instance.model.findElement(resource.parent) as IErlFolder)
                if (folder !== null && folder.onSourcePath) {
                    foundAppSrc = true
                }
            }
            !foundAppSrc
        ]
        if (! foundAppSrc) {
            MarkerUtils::addMarker(null, project, null, "No .app.src file found, can't compile with rebar", -1,
                IMarker::SEVERITY_ERROR, IMarker::PROBLEM)
        }
    }

    override getId() {
        ErlangCore::PLUGIN_ID + '.rebar.builder'
    }
    
}
