package org.erlide.core.builder;

import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.runtime.IProgressMonitor;

public interface IBuilder {

    IProject[] build(int kind, Map<String, String> args,
            IResourceDelta delta, IProgressMonitor monitor);

    void clean(IProgressMonitor monitor);

}
