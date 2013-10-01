package org.erlide.core.internal.builder

import org.eclipse.core.resources.IProject
import org.erlide.core.builder.IBuilder
import static extension org.erlide.core.internal.builder.ErlangToolExtensions.*
import org.erlide.util.ErlLogger

class BuilderFactory {

    def IBuilder getBuilderFor(IProject project) {
        if (project.hasMakefile) {
            ErlLogger.debug("Use make builder")
            return new MakeBuilder(project)
        }
        if (project.hasRebarConfig) {
            ErlLogger.debug("Use rebar builder")
            return new RebarBuilder(project)
        }
        ErlLogger.debug("Use internal builder")
        return new InternalBuilder(project)
    }

}
