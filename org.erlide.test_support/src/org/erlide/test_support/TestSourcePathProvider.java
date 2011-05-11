package org.erlide.test_support;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IProject;
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
import org.erlide.core.common.SourcePathProvider;
import org.erlide.jinterface.ErlLogger;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

public class TestSourcePathProvider implements SourcePathProvider,
        IResourceChangeListener {

    Map<IProject, Set<IPath>> pathsMap;

    public TestSourcePathProvider() {
        pathsMap = Maps.newHashMap();
        try {
            computeSourcePaths();
        } catch (final CoreException e) {
            ErlLogger.warn(e);
            pathsMap = Maps.newHashMap();
        }
        // System.out.println("## paths=" + paths);
        final IWorkspace workspace = ResourcesPlugin.getWorkspace();
        workspace.addResourceChangeListener(this,
                IResourceChangeEvent.POST_CHANGE);
    }

    public Collection<IPath> getSourcePathsForModel(final IProject project) {
        return getProjectPaths(project);
    }

    public Collection<IPath> getSourcePathsForBuild(final IProject project) {
        return getProjectPaths(project);
    }

    public Collection<IPath> getSourcePathsForExecution(final IProject project) {
        return getProjectPaths(project);
    }

    public Collection<IPath> getIncludePaths(final IProject project) {
        return getProjectPaths(project);
    }

    private void computeSourcePaths() throws CoreException {
        ResourcesPlugin.getWorkspace().getRoot().accept(new IResourceVisitor() {

            public boolean visit(final IResource resource) throws CoreException {
                final IProject project = resource.getProject();
                if (project != null && isTestDir(resource)) {
                    final Set<IPath> ps = getProjectPaths(project);
                    ps.add(resource.getLocation());
                    pathsMap.put(project, ps);
                }
                return true;
            }
        });
    }

    private Set<IPath> getProjectPaths(final IProject project) {
        Set<IPath> ps = pathsMap.get(project);
        if (ps == null) {
            ps = Sets.newHashSet();
        }
        return ps;
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
                    final Set<IPath> paths = getProjectPaths(resource
                            .getProject());
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
