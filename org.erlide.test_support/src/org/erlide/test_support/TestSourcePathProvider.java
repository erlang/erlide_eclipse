package org.erlide.test_support;

import java.util.Collection;
import java.util.Set;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.erlide.core.erlang.util.SourcePathProvider;
import org.erlide.jinterface.util.ErlLogger;

import com.google.common.collect.Sets;

public class TestSourcePathProvider implements SourcePathProvider,
        IResourceChangeListener {

    Set<IPath> paths;

    public TestSourcePathProvider() {
        try {
            paths = computeSourcePaths();
        } catch (final CoreException e) {
            ErlLogger.warn(e);
            paths = Sets.newHashSet();
        }
        // System.out.println("## paths=" + paths);
        final IWorkspace workspace = ResourcesPlugin.getWorkspace();
        workspace.addResourceChangeListener(this,
                IResourceChangeEvent.POST_CHANGE);
    }

    public Collection<IPath> getSourcePaths() {
        return paths;
    }

    private Set<IPath> computeSourcePaths() throws CoreException {
        final Set<IPath> result = Sets.newHashSet();
        ResourcesPlugin.getWorkspace().getRoot().accept(new IResourceVisitor() {

            public boolean visit(final IResource resource) throws CoreException {
                if (isTestDir(resource)) {
                    result.add(resource.getLocation());
                }
                return true;
            }

        });
        return result;
    }

    public void resourceChanged(final IResourceChangeEvent event) {
        // TODO keep 'paths' updated

        final IResourceDelta delta = event.getDelta();
        if (delta == null) {
            return;
        }
        // System.out.println("@@ >> BterlSrcPathProvider: resources updated...");
        try {
            delta.accept(new IResourceDeltaVisitor() {
                public boolean visit(final IResourceDelta theDelta)
                        throws CoreException {
                    final IResource resource = theDelta.getResource();
                    final IContainer parent = resource.getParent();
                    if (parent == null) {
                        return true;
                    }
                    // TODO isintestpath is slow...
                    final IPath parentLocation = parent.getLocation();
                    if (theDelta.getKind() == IResourceDelta.ADDED
                            && !paths.contains(parentLocation)
                            && isTestDir(parent)) {
                        paths.add(parentLocation);

                    }
                    if (theDelta.getKind() == IResourceDelta.REMOVED
                            && paths.contains(parentLocation)) {
                        paths.remove(parentLocation);
                    }
                    return true;
                }
            });
        } catch (final CoreException e) {
            e.printStackTrace();
        }
    }

    static boolean isTestDir(final IResource resource) {
        if (!(resource instanceof IContainer)) {
            return false;
        }
        if (resource.getName().equals("garbage")) {
            return false;
        }
        if (resource.getName().equals("lost+found")) {
            return false;
        }
        try {
            for (final IResource res : ((IContainer) resource).members()) {
                if (res.getName().contains("_SUITE.erl")) {
                    return true;
                }
            }
        } catch (final CoreException e) {
        }
        return false;
    }
}
