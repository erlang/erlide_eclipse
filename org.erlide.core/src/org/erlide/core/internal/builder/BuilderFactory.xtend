package org.erlide.core.internal.builder

import org.eclipse.core.resources.IProject
import org.erlide.core.builder.IBuilder
import org.erlide.engine.model.root.OldErlangProjectProperties
import org.erlide.util.ErlLogger

class BuilderFactory {

    static val MAKE_BUILDER = "make"
    static val EMAKE_BUILDER = "emake"
    static val REBAR_BUILDER = "rebar"

    def IBuilder getBuilderFor(IProject project) {
        val properties = new OldErlangProjectProperties(project);
        val builder = properties.builderProperties.get("builder")
        switch (builder) {
            case MAKE_BUILDER: {
                ErlLogger.trace("builder", MAKE_BUILDER)
                new MakeBuilder(project)
            }
            case EMAKE_BUILDER: {
                ErlLogger.trace("builder", EMAKE_BUILDER)
                new EmakeBuilder(project)
            }
            case REBAR_BUILDER: {
                ErlLogger.trace("builder", REBAR_BUILDER)
                new RebarBuilder(project)
            }
            default: {
                ErlLogger.trace("builder", "internal")
                new InternalBuilder(project)
            }
        }
    }

}
