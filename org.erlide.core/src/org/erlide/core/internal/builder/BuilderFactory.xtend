package org.erlide.core.internal.builder

import org.eclipse.core.resources.IProject
import org.erlide.core.builder.IBuilder
import org.erlide.util.ErlLogger

import static extension org.erlide.core.internal.builder.ErlangToolExtensions.*

class BuilderFactory {

    def IBuilder getBuilderFor(IProject project) {
        if (project.buildsWithMake) {
            ErlLogger.trace("builder", "make")
            return new MakeBuilder(project)
        }
        if (project.buildsWithEmake) {
            ErlLogger.trace("builder", "emake")
            return new EmakeBuilder(project)
        }
        if (project.buildsWithRebar) {
            ErlLogger.trace("builder", "rebar")
            return new RebarBuilder(project)
        }
        ErlLogger.trace("builder", "internal")
        return new InternalBuilder(project)
    }

}
