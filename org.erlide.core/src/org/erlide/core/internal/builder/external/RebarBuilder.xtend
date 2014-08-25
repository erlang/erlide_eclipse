package org.erlide.core.internal.builder.external

import org.eclipse.core.runtime.CoreException
import org.erlide.core.internal.builder.BuildNotifier
import org.erlide.core.internal.builder.ExternalBuilder
import org.erlide.engine.model.builder.BuilderTool
import org.erlide.engine.model.root.IErlProject

class RebarBuilder extends ExternalBuilder {

    new() {
        super(BuilderTool.REBAR)
    }

    override build(BuildKind kind, IErlProject erlProject, BuildNotifier notifier) throws CoreException {
        val result = super.build(kind, erlProject, notifier)
        result
    }

    override clean(IErlProject erlProject,BuildNotifier notifier) {
        super.clean(erlProject, notifier)
    }

    override getProperties() {
        null
    }

}
