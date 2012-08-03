package org.erlide.shade.bterl.builder;

import java.io.File;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileInfo;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourceAttributes;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IStreamListener;
import org.eclipse.debug.core.model.IStreamMonitor;
import org.eclipse.debug.internal.core.StreamsProxy;
import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendException;
import org.erlide.backend.BackendUtils;
import org.erlide.backend.IBackend;
import org.erlide.core.services.builder.BuildResource;
import org.erlide.core.services.builder.BuilderHelper;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.IRpcFuture;
import org.erlide.jinterface.rpc.RpcException;
import org.erlide.shade.bterl.ui.launcher.TestLaunchDelegate;
import org.erlide.utils.ErlUtils;
import org.erlide.utils.SystemUtils;

import com.ericsson.otp.erlang.OtpErlang;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.base.Charsets;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

@SuppressWarnings("restriction")
public class TestCodeBuilder extends IncrementalProjectBuilder {

    private final BuilderHelper helper = new BuilderHelper();

    public static final String BUILDER_ID = "shade.bterl.builder";
    private static final String MARKER_TYPE = "org.erlide.test_support.bterlProblem";
    private static final boolean DEBUG = Boolean.parseBoolean(System
            .getProperty("erlide.test_builder.debug"));

    static void addMarker(final IResource file, final String message,
            int lineNumber, final int severity) {
        try {
            final IMarker marker = file.createMarker(MARKER_TYPE);
            marker.setAttribute(IMarker.MESSAGE, message);
            marker.setAttribute(IMarker.SEVERITY, severity);
            if (lineNumber == -1) {
                lineNumber = 1;
            }
            marker.setAttribute(IMarker.LINE_NUMBER, lineNumber);
        } catch (final CoreException e) {
        }
    }

    @SuppressWarnings("rawtypes")
    @Override
    protected IProject[] build(final int kind, final Map args,
            final IProgressMonitor monitor) throws CoreException {
        final IProject project = getProject();
        if (DEBUG) {
            ErlLogger.info("##### start test builder (%s) %s",
                    helper.buildKind(kind), project.getName());
        }
        final long time = System.currentTimeMillis();
        if (kind == FULL_BUILD) {
            fullBuild(monitor);
        } else {
            final IResourceDelta delta = getDelta(project);
            if (delta == null) {
                fullBuild(monitor);
            } else {
                incrementalBuild(delta, monitor);
            }
        }
        if (DEBUG) {
            ErlLogger.info("##### done test builder %s took %s",
                    project.getName(),
                    Long.toString(System.currentTimeMillis() - time));
        }
        return null;
    }

    @Override
    protected void clean(final IProgressMonitor monitor) throws CoreException {
        final IProject project = getProject();
        project.deleteMarkers(MARKER_TYPE, true, IResource.DEPTH_INFINITE);
        final Set<BuildResource> resourcesToBuild = getResourcesToBuild(
                project, monitor, true);
        for (final BuildResource res : resourcesToBuild) {
            // resources are already deleted by getResourcesToBuild...
            // TODO fix bterl clean
            ErlLogger.info("TODO: CLEAN: " + res.getResource().getName());
        }
    }

    void checkFile(final IResource resource) {
        if (resource instanceof IFile && resource.getName().endsWith(".erl")) {
            final IFile file = (IFile) resource;
            deleteMarkers(file);
            if (DEBUG) {
                ErlLogger.debug(" >>> bterl build ::: " + file.getName());
            }
        }
    }

    private void deleteMarkers(final IResource file) {
        try {
            file.deleteMarkers(MARKER_TYPE, false, IResource.DEPTH_ZERO);
        } catch (final CoreException ce) {
        }
    }

    protected void fullBuild(final IProgressMonitor monitor)
            throws CoreException {
        final IProject project = getProject();
        project.deleteMarkers(MARKER_TYPE, true, IResource.DEPTH_INFINITE);
        checkForMakeLinks(project, monitor);
        final Set<BuildResource> resourcesToBuild = getResourcesToBuild(
                project, monitor, false);
        if (DEBUG) {
            ErlLogger.debug("resources to build: " + resourcesToBuild.size());
        }
        doBuild(project, resourcesToBuild, false, monitor);
    }

    private void doBuild(final IProject project,
            final Set<BuildResource> resourcesToBuild,
            final boolean deleteMarkers, final IProgressMonitor monitor) {
        try {
            final Map<IRpcFuture, IResource> results = Maps.newHashMap();
            IBackend backend;
            try {
                backend = BackendCore.getBackendManager().getBuildBackend(
                        project);
            } catch (final BackendException e) {
                backend = null;
            }
            if (backend == null) {
                final String message = "No backend with the required "
                        + "version could be found. Can't build.";
                // MarkerHelper.addProblemMarker(project, null, message, 0,
                // IMarker.SEVERITY_ERROR);
                throw new BackendException(message);
            }
            registerBterlBeams(backend);
            for (final BuildResource bres : resourcesToBuild) {
                if (monitor.isCanceled()) {
                    return;
                }
                final IResource resource = bres.getResource();
                resource.deleteMarkers(MARKER_TYPE, true, IResource.DEPTH_ZERO);

                final List<OtpErlangObject> incs = Lists.newArrayList();
                for (final String path : TestLaunchDelegate.getBterlPath()) {
                    incs.add(ErlUtils.format("{i, ~s}", path));
                }
                final OtpErlangList compilerOptions = OtpErlang.mkList(incs
                        .toArray(new OtpErlangObject[incs.size()]));

                final String outputDir = bres.getResource().getParent()
                        .getProjectRelativePath().toString();
                IRpcFuture f = null;
                f = helper.startCompileErl(project, bres, outputDir, backend,
                        compilerOptions, false);
                if (f != null) {
                    results.put(f, resource);
                }
            }
            final List<Entry<IRpcFuture, IResource>> done = Lists
                    .newArrayList();
            final List<Entry<IRpcFuture, IResource>> waiting = Lists
                    .newArrayList(results.entrySet());

            // TODO should use some kind of notification!
            while (waiting.size() > 0) {
                for (final Entry<IRpcFuture, IResource> entry : waiting) {
                    if (monitor.isCanceled()) {
                        return;
                    }
                    OtpErlangObject result;
                    try {
                        result = entry.getKey().get(10);
                    } catch (final Exception e) {
                        result = null;
                    }
                    if (result != null) {
                        final IResource resource = entry.getValue();
                        helper.completeCompile(project, resource, result,
                                backend, new OtpErlangList());
                        done.add(entry);
                    }
                }
                waiting.removeAll(done);
                done.clear();
            }
            unregisterBterlBeams(backend);

        } catch (final OperationCanceledException e) {
            if (BuilderHelper.isDebugging()) {
                ErlLogger.debug("Build of " + project.getName()
                        + " was canceled.");
            }
        } catch (final Exception e) {
            ErlLogger.error(e);
            // final String msg = NLS.bind(
            // BuilderMessages.build_inconsistentProject,
            // e.getLocalizedMessage());
            // MarkerHelper.addProblemMarker(project, null, msg, 0,
            // IMarker.SEVERITY_ERROR);
        }
    }

    public void registerBterlBeams(final IBackend backend)
            throws CoreException, RpcException {
        final String[] bterl_beams = TestLaunchDelegate.getBterlPath();
        for (final String bterl_beam : bterl_beams) {
            backend.call("code", "add_path", "s", bterl_beam);
        }
    }

    public void unregisterBterlBeams(final IBackend backend)
            throws CoreException, RpcException {
        final String[] bterl_beams = TestLaunchDelegate.getBterlPath();
        for (final String bterl_beam : bterl_beams) {
            backend.call("code", "del_path", "s", bterl_beam);
        }
    }

    private void checkForMakeLinks(final IProject project,
            final IProgressMonitor monitor) throws CoreException {
        project.accept(new MakeLinksVisitor(monitor));
    }

    protected void incrementalBuild(final IResourceDelta delta,
            final IProgressMonitor monitor) throws CoreException {
        final IProject project = getProject();
        final Set<BuildResource> resourcesToBuild = getResourcesToBuild(delta,
                monitor);
        doBuild(project, resourcesToBuild, true, monitor);
    }

    private Set<BuildResource> getResourcesToBuild(final IResourceDelta delta,
            final IProgressMonitor monitor) throws CoreException {
        Set<BuildResource> resourcesToBuild = Sets.newHashSet();
        final IProgressMonitor submon = new SubProgressMonitor(monitor, 10);
        submon.beginTask("retrieving resources to build",
                IProgressMonitor.UNKNOWN);
        resourcesToBuild = getAffectedResources(delta, submon);
        submon.done();
        return resourcesToBuild;
    }

    private Set<BuildResource> getResourcesToBuild(
            final IProject currentProject, final IProgressMonitor monitor,
            final boolean clean) throws CoreException {
        Set<BuildResource> resourcesToBuild = Sets.newHashSet();
        final IProgressMonitor submon = new SubProgressMonitor(monitor, 10);
        submon.beginTask("retrieving resources to build",
                IProgressMonitor.UNKNOWN);
        resourcesToBuild = getAffectedResources(currentProject, submon, clean);
        submon.done();
        return resourcesToBuild;
    }

    public static Set<BuildResource> getAffectedResources(
            final IProject project, final IProgressMonitor monitor,
            final boolean clean) throws CoreException {
        final Set<BuildResource> result = Sets.newHashSet();
        project.accept(new BterlResourceVisitor(result, monitor, clean));
        return result;
    }

    private Set<BuildResource> getAffectedResources(final IResourceDelta delta,
            final IProgressMonitor monitor) throws CoreException {
        final Set<BuildResource> result = Sets.newHashSet();
        delta.accept(new BterlDeltaVisitor(result, monitor));
        return result;
    }

    private static class BterlResourceVisitor implements IResourceVisitor {

        private final Set<BuildResource> result;
        private final IProgressMonitor monitor;
        private final boolean cleaning;

        public BterlResourceVisitor(final Set<BuildResource> result,
                final IProgressMonitor monitor, final boolean clean) {
            this.result = result;
            this.monitor = monitor;
            cleaning = clean;
        }

        @Override
        public boolean visit(final IResource resource) throws CoreException {
            final IProject my_project = resource.getProject();
            if (resource.isDerived()) {
                return true;
            }
            if (cleaning) {
                if (resource.getType() == IResource.FILE
                        && "beam".equals(resource.getFileExtension())
                        && isInTestPath(resource, my_project)
                        && resource.getResourceAttributes() != null
                        && !resource.getResourceAttributes().isReadOnly()) {
                    resource.delete(true, new SubProgressMonitor(monitor, 1));
                    monitor.worked(1);
                }
            } else {
                if (resource.getType() == IResource.FILE
                        && "erl".equals(resource.getFileExtension())
                        && isInTestPath(resource, my_project)) {
                    try {
                        final ResourceAttributes a = resource
                                .getResourceAttributes();
                        if (!a.isSymbolicLink()) {
                            final BuildResource bres = new BuildResource(
                                    resource, resource.getParent()
                                            .getLocation().toString());
                            result.add(bres);
                            monitor.worked(1);
                        }

                    } catch (final Exception e) {
                        e.printStackTrace();
                    }
                }
            }
            // return true to continue visiting children.
            return true;
        }
    }

    private static class BterlDeltaVisitor implements IResourceDeltaVisitor {

        private final Set<BuildResource> result;
        private final IProgressMonitor monitor;

        public BterlDeltaVisitor(final Set<BuildResource> result,
                final IProgressMonitor monitor) {
            this.result = result;
            this.monitor = monitor;
        }

        @Override
        public boolean visit(final IResourceDelta delta) throws CoreException {
            final IResource resource = delta.getResource();
            if (resource.isDerived()) {
                return true;
            }
            final IProject my_project = resource.getProject();
            if (resource.getType() == IResource.FILE
                    && "erl".equals(resource.getFileExtension())
                    && isInTestPath(resource, my_project)) {
                try {
                    final ResourceAttributes a = resource
                            .getResourceAttributes();
                    if (a != null && !a.isSymbolicLink()) {
                        final BuildResource bres = new BuildResource(resource,
                                resource.getParent().getLocation().toString());
                        result.add(bres);
                        monitor.worked(1);
                    }

                } catch (final Exception e) {
                    e.printStackTrace();
                }
                return false;
            }
            if (resource.getLocation().toString().contains("lost+found")) {
                return false;
            }
            // if (resource.getType() == IResource.FOLDER) {
            // MPS has a link that creates a loop
            final ResourceAttributes a = resource.getResourceAttributes();
            if (a != null && a.isSymbolicLink()) {
                final File f = new File(resource.getLocation().toString());
                final IFileInfo info = EFS.getFileSystem(EFS.SCHEME_FILE)
                        .fromLocalFile(f).fetchInfo();
                final String target = info
                        .getStringAttribute(EFS.ATTRIBUTE_LINK_TARGET);
                return target == null
                        || !resource.getLocation().toString().contains(target)
                        && target.contains("/");
            }
            return true;
        }

    }

    private static class MakeLinksVisitor implements IResourceVisitor {

        private static final String MAKE_LINKS_MSG = "Project %s uses deprecated 'make_links' scripts."
                + " Please use symbolic links instead";
        private final IProgressMonitor monitor;

        public MakeLinksVisitor(final IProgressMonitor monitor) {
            this.monitor = monitor;
        }

        private void runMakeLinks(final IContainer container) {
            try {
                // if (hasLinks(container)) {
                // return;
                // }
                File dir = new File(container.getRawLocationURI());
                if (!dir.isDirectory()) {
                    dir = dir.getParentFile();
                }
                if (DEBUG) {
                    ErlLogger.debug("running make_links in "
                            + dir.getAbsolutePath());
                }
                final Process makeLinks = DebugPlugin.exec(
                        new String[] { "./make_links" }, dir);
                final StreamsProxy proxy = new StreamsProxy(makeLinks,
                        Charsets.ISO_8859_1.name());
                final IStreamListener listener = new IStreamListener() {
                    @Override
                    public void streamAppended(final String text,
                            final IStreamMonitor streamMonitor) {
                        final String[] lines = text.split("\n");
                        for (final String line : lines) {
                            System.out.println("make_links>> " + line);
                        }
                    }
                };
                if (SystemUtils.hasFeatureEnabled("erlide.make_links.snoop")) {
                    proxy.getOutputStreamMonitor().addListener(listener);
                    proxy.getErrorStreamMonitor().addListener(listener);
                }
                while (true) {
                    try {
                        makeLinks.waitFor();
                        break;
                    } catch (final InterruptedException e1) {
                    }
                }
                proxy.kill();
                container.refreshLocal(IResource.DEPTH_INFINITE,
                        new NullProgressMonitor());
            } catch (final CoreException e) {
                // there is no make_links
            }
        }

        @SuppressWarnings("unused")
        private boolean hasLinks(final IContainer container)
                throws CoreException {
            boolean haslinks = false;
            for (final IResource f : container.members()) {
                if (f.isLinked()) {
                    haslinks = true;
                    break;
                }
            }
            return haslinks;
        }

        @Override
        public boolean visit(final IResource resource) throws CoreException {
            final IProject my_project = resource.getProject();
            if (resource.isDerived()) {
                return true;
            }
            if (resource.getType() == IResource.FILE
                    && "make_links".equals(resource.getName())) {
                if (isInTestPath(resource, my_project)) {
                    final String msg = String.format(MAKE_LINKS_MSG,
                            my_project.getName());
                    addMarker(resource, msg, 0, IMarker.SEVERITY_WARNING);
                    runMakeLinks(resource.getParent());
                    monitor.worked(1);
                }
            }
            // return true to continue visiting children.
            return true;
        }
    }

    /**
     * A resource is in a test directory if it has a sibling with name matching
     * "*_SUITE.erl" and if it has an Erlang source directory as an ancestor.
     * 
     * @param resource
     * @param myProject
     * @return
     */
    public static boolean isInTestPath(final IResource resource,
            final IProject myProject) {
        try {
            if (!underSourcePath(resource, myProject)) {
                return false;
            }
            final IContainer parent = resource.getParent();
            if (parent.getFullPath().toPortableString().contains("garbage")) {
                return false;
            }
            if (parent.getFullPath().toPortableString().contains("lost+found")) {
                return false;
            }
            final IResource[] siblings = parent.members();
            for (final IResource res : siblings) {
                if (res.getName().contains("_SUITE.erl")) {
                    return true;
                }
            }
            return false;
        } catch (final CoreException e) {
            return false;
        }
    }

    private static boolean underSourcePath(final IResource resource,
            final IProject myProject) {
        final Collection<IPath> srcDirs = BackendUtils
                .getExtraSourcePathsForBuild(myProject);
        final IPath rpath = resource.getFullPath().removeFirstSegments(1);
        for (final IPath src : srcDirs) {
            final IPath srcPath = src
                    .removeFirstSegments(rpath.segmentCount() - 1);
            if (srcPath.isPrefixOf(rpath)) {
                return true;
            }
        }
        return false;
    }
}
