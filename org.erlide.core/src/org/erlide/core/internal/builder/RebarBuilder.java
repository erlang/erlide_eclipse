package org.erlide.core.internal.builder;

import java.util.Map;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.builder.MarkerUtils;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.root.BuilderConfigParser;
import org.erlide.engine.model.root.IErlFolder;

public class RebarBuilder extends ExternalBuilder {

    @Override
    public String getOsCommand() {
        return "rebar";
    }

    @Override
    public BuilderConfigParser getConfigParser() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public IProject[] build(final int kind, final Map<String, String> args,
            final IProgressMonitor monitor) throws CoreException {
        final IProject[] result = super.build(kind, args, monitor);
        checkIfProjectHasAppFile();
        return result;
    }

    private boolean foundAppSrc;

    private void checkIfProjectHasAppFile() throws CoreException {
        foundAppSrc = false;
        getProject().accept(new IResourceVisitor() {
            @Override
            public boolean visit(final IResource resource) throws CoreException {
                if (resource.getName().endsWith(".app.src")) {
                    final IErlFolder folder = (IErlFolder) ErlangEngine
                            .getInstance().getModel()
                            .findElement(resource.getParent());
                    if (folder != null && folder.isOnSourcePath()) {
                        foundAppSrc = true;
                    }
                }
                return !foundAppSrc;
            }
        });
        if (!foundAppSrc) {
            MarkerUtils.addMarker(null, getProject(), null,
                    "No .app.src file found, can't compile with rebar", -1,
                    IMarker.SEVERITY_ERROR, IMarker.PROBLEM);
        }
    }

}
