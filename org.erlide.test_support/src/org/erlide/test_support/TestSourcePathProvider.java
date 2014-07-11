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
import org.erlide.engine.model.SourcePathProvider;
import org.erlide.util.ErlLogger;
import org.erlide.util.SystemConfiguration;

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

        final IWorkspace workspace = ResourcesPlugin.getWorkspace();
        workspace.addResourceChangeListener(this, IResourceChangeEvent.POST_CHANGE);
    }

    @Override
    public Collection<IPath> getSourcePathsForModel(final IProject project) {
        return getProjectPaths(project);
    }

    @Override
    public Collection<IPath> getSourcePathsForBuild(final IProject project) {
        return getProjectPaths(project);
    }

    @Override
    public Collection<IPath> getSourcePathsForExecution(final IProject project) {
        return getProjectPaths(project);
    }

    @Override
    public Collection<IPath> getIncludePaths(final IProject project) {
        return getProjectPaths(project);
    }

    private void computeSourcePaths() throws CoreException {
        ResourcesPlugin.getWorkspace().getRoot().accept(new IResourceVisitor() {

            @Override
            public boolean visit(final IResource resource) throws CoreException {
                final IProject project = resource.getProject();
                if (project != null && isTestDir(resource)) {
                    final Set<IPath> ps = getProjectPaths(project);
                    ps.add(resource.getProjectRelativePath());
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

    @Override
    public void resourceChanged(final IResourceChangeEvent event) {
        // TODO keep 'paths' updated
        final IResourceDelta delta = event.getDelta();
        if (delta == null) {
            return;
        }

        try {
            final long time = System.currentTimeMillis();
            delta.accept(new IResourceDeltaVisitor() {
                @Override
                public boolean visit(final IResourceDelta theDelta) throws CoreException {
                    final IResource resource = theDelta.getResource();
                    if (!(resource instanceof IContainer)) {
                        return false;
                    }
                    final IContainer container = (IContainer) resource;
                    final IPath location = container.getLocation();
                    final Set<IPath> paths = getProjectPaths(resource.getProject());
                    if (theDelta.getKind() == IResourceDelta.ADDED
                            && !paths.contains(location) && isTestDir(container)) {
                        paths.add(location);
                    }
                    if (theDelta.getKind() == IResourceDelta.REMOVED
                            && paths.contains(location)) {
                        paths.remove(location);
                    }
                    return true;
                }
            });
            if (SystemConfiguration.hasFeatureEnabled("erlide.debug.tspp")) {
                ErlLogger.debug("TSPP took " + (System.currentTimeMillis() - time));
            }
        } catch (final CoreException e) {
            ErlLogger.error(e);
        }
    }

    static boolean isTestDir(final IResource resource) {
        if (!(resource instanceof IContainer)) {
            return false;
        }
        final String path = resource.getFullPath().toPortableString();
        if (path.contains("garbage")) {
            return false;
        }
        if (path.contains("lost+found")) {
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
