package org.erlide.core.internal.builder;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.erlide.backend.BackendCore;
import org.erlide.backend.api.IBackend;
import org.erlide.core.builder.MarkerUtils;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.root.BuilderConfigParser;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.util.ErlLogger;

public class EmakeBuilder extends ExternalBuilder {

    @Override
    public String getOsCommand() {
        final IBackend backend = BackendCore.getBackendManager()
                .getBuildBackend(getProject());
        final IPath path = new Path(backend.getRuntimeInfo().getOtpHome())
                .append("bin/erl");
        return path.toOSString();
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
                for (final IResource f : bf.members()) {
                    try {
                        f.delete(true, null);
                    } catch (final CoreException e) {
                        ErlLogger.warn("Could not clean up output directory "
                                + bf.getLocation());
                    }
                }
            } catch (final CoreException e) {
                // ignore
            }
        }
    }

}
