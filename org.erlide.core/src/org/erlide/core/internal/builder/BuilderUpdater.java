package org.erlide.core.internal.builder;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.runtime.CoreException;
import org.erlide.core.ErlangCore;

public class BuilderUpdater implements IResourceChangeListener {

    @Override
    public void resourceChanged(final IResourceChangeEvent event) {
        if (event == null || event.getDelta() == null) {
            return;
        }
        try {
            event.getDelta().accept(new IResourceDeltaVisitor() {
                @Override
                public boolean visit(final IResourceDelta delta)
                        throws CoreException {
                    final IResource resource = delta.getResource();
                    if (!(resource instanceof IProject)) {
                        return true;
                    }
                    final IProject project = (IProject) resource;
                    if (project.hasNature(ErlangCore.NATURE_ID)
                            && (delta.getFlags() & IResourceDelta.OPEN) == IResourceDelta.OPEN) {
                        processProject(project);
                    }
                    return false;
                }

            });
        } catch (final CoreException e) {
            e.printStackTrace();
        }
    }

    private void processProject(final IProject project) {
        if (project.getResourceAttributes().isReadOnly()) {
            return;
        }

        // add TodoBuilder

    }
}
