package org.erlide.core.builder.external;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.erlide.core.builder.BuildNotifier;
import org.erlide.core.builder.ErlangBuilder;
import org.erlide.core.builder.ExternalBuilder;
import org.erlide.engine.model.builder.BuilderProperties;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.IErlProject;

@SuppressWarnings("all")
public class RebarBuilder extends ExternalBuilder {
    public RebarBuilder() {
        super(BuilderTool.REBAR);
    }

    @Override
    public IProject[] build(final ErlangBuilder.BuildKind kind,
            final IErlProject erlProject, final BuildNotifier notifier)
            throws CoreException {
        IProject[] _xblockexpression = null;
        {
            final IProject[] result = super.build(kind, erlProject, notifier);
            _xblockexpression = result;
        }
        return _xblockexpression;
    }

    @Override
    public void clean(final IErlProject erlProject, final BuildNotifier notifier) {
        super.clean(erlProject, notifier);
    }

    @Override
    public BuilderProperties getProperties() {
        return null;
    }
}
