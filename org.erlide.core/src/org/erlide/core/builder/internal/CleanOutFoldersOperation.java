// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package org.erlide.core.builder.internal;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceProxy;
import org.eclipse.core.resources.IResourceProxyVisitor;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.core.erlang.util.ResourceUtil;

/**
 * <p>
 * Operation for cleaning output and binary folders of an Erlang project.
 * </p>
 * 
 * @author Leif Frenzel
 * @author Andrei Formiga
 */
class CleanOutFoldersOperation implements IWorkspaceRunnable {

    private static final IResourceProxyVisitor folderCleaner = new FolderCleaner();

    private final IProject project;

    CleanOutFoldersOperation(final IProject project) {
        this.project = project;
    }

    public void run(final IProgressMonitor mon) throws CoreException {
        mon.beginTask("Cleaning output folder", 15);
        try {
            shrubOutFolder(mon); // (15)
        } finally {
            mon.done();
        }
    }

    // helping methods
    // ////////////////

    private void shrubOutFolder(final IProgressMonitor mon)
            throws CoreException {
        mon.subTask("Shrubbing output folder.");
        final IContainer outFolder = ResourceUtil.getOutFolder(project);
        if (outFolder != null && !outFolder.equals(project)) {
            outFolder.accept(folderCleaner, IContainer.INCLUDE_PHANTOMS);
        }
        mon.worked(15);
    }

    // inner classes
    // //////////////

    static class FolderCleaner implements IResourceProxyVisitor {

        public boolean visit(final IResourceProxy proxy) throws CoreException {
            if (proxy.getType() == IResource.FILE) {
                final IResource resource = proxy.requestResource();
                System.err.println("Deleting " + resource.toString());
                resource.delete(IResource.FORCE, null);
            }
            return true;
        }
    }
}
