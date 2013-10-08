package org.erlide.core.internal.builder;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.builder.MarkerUtils;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.root.BuilderConfigParser;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.util.ErlLogger;

public class EmakeBuilder extends ExternalBuilder {

    @Override
    public String getOsCommand() {
        return "erl";
    }

    @Override
    protected String getCompileTarget() {
        return "-make";
    }

    @Override
    protected String getCleanTarget() {
        return null;
    }

    @Override
    public BuilderConfigParser getConfigParser() {
        return null;
    }

    @Override
    public void clean(final IProgressMonitor monitor) {
        final IProject project = getProject();
        MarkerUtils.removeProblemMarkersFor(project);

        final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                .getErlangProject(project);
        final IFolder bf = project.getFolder(erlProject.getOutputLocation());
        if (bf.exists()) {
            try {
                bf.delete(true, monitor);
            } catch (final CoreException e) {
                ErlLogger.warn("Could not clean up output directory "
                        + bf.getLocation());
            }
        }
    }

}
