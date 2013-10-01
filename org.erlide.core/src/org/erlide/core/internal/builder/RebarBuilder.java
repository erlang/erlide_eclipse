package org.erlide.core.internal.builder;

import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.builder.IBuilder;

public class RebarBuilder implements IBuilder {

    private final IProject project;

    public RebarBuilder(final IProject project) {
        this.project = project;
    }

    @Override
    public IProject[] build(final int kind, final Map<String, String> args,
            final IResourceDelta delta, final IProgressMonitor monitor) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void clean(final IProgressMonitor monitor) {
        // TODO Auto-generated method stub

    }

}
