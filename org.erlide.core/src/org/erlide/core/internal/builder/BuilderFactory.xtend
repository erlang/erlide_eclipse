package org.erlide.core.internal.builder

import org.eclipse.core.resources.IProject
import org.erlide.core.builder.IBuilder
import static extension org.erlide.core.internal.builder.ErlangToolExtensions.*
import org.erlide.util.ErlLogger

class BuilderFactory {

    def IBuilder getBuilderFor(IProject project) {
        if (project.hasMakefile) {
            ErlLogger.trace("builder", "make")
            return new MakeBuilder(project)
        }
        if (project.hasRebarConfig) {
            ErlLogger.trace("builder", "rebar")
            return new RebarBuilder(project)
        }
        ErlLogger.trace("builder", "internal")
        return new InternalBuilder(project)
    }

}
