package org.erlide.core.internal.builder;

import org.eclipse.core.resources.IProject;

public class RebarBuilder extends ExternalBuilder {

    public RebarBuilder(final IProject project) {
        super(project, "rebar");
    }

}
