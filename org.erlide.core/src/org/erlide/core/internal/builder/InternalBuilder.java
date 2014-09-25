/*******************************************************************************
 * Copyright (c) 2013 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.internal.builder;

import static com.google.common.collect.Lists.newArrayList;

import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.osgi.util.NLS;
import org.erlide.backend.BackendCore;
import org.erlide.backend.api.BackendException;
import org.erlide.backend.api.IBackend;
import org.erlide.core.builder.BuildResource;
import org.erlide.core.builder.BuilderHelper;
import org.erlide.core.builder.BuilderHelper.SearchVisitor;
import org.erlide.core.builder.CompilerOptions;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.builder.BuilderProperties;
import org.erlide.engine.model.builder.MarkerUtils;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.SourceKind;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.rpc.RpcFuture;
import org.erlide.util.ErlLogger;
import org.erlide.util.SystemConfiguration;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public class InternalBuilder extends ErlangBuilder {

    private final BuilderHelper helper = new BuilderHelper();
    IResourceDelta delta;

    public void setDelta(final IResourceDelta delta) {
        this.delta = delta;
    }

    @Override
    public IProject[] build(final BuildKind kind, final IErlProject erlProject,
            final BuildNotifier notifier) throws CoreException {

        final long time = System.currentTimeMillis();
        final IProject project = erlProject.getWorkspaceProject();
        if (project == null || !project.isAccessible()) {
            return null;
        }

        if (BuilderHelper.isDebugging()) {
            ErlLogger.trace("build", "Start " + project.getName() + ": " + kind);
        }
        try {
            initializeBuilder(notifier);

            // TODO validate source and include directories
            final ErlangProjectProperties properties = erlProject.getProperties();
            final IPath out = properties.getOutputDir();
            final IResource outr = project.findMember(out);
            if (outr != null) {
                try {
                    outr.setDerived(true, null);
                    outr.refreshLocal(IResource.DEPTH_ZERO, null);
                } catch (final CoreException e) {
                    // ignore it
                }
            }

            if (delta != null && delta.getAffectedChildren().length != 0) {
                handleAppFile(project, project.getLocation().toPortableString() + "/"
                        + out, properties.getSourceDirs());
            }
            handleErlangFiles(erlProject, project, kind, delta, notifier);
        } catch (final OperationCanceledException e) {
            if (BuilderHelper.isDebugging()) {
                ErlLogger.debug("Build of " + project.getName() + " was canceled.");
            }
        } catch (final Exception e) {
            ErlLogger.error(e);
            final String msg = NLS.bind(BuilderMessages.build_inconsistentProject,
                    e.getLocalizedMessage(), e.getClass().getName());
            MarkerUtils
                    .createProblemMarker(project, null, msg, 0, IMarker.SEVERITY_ERROR);
        } finally {
            cleanup(notifier);
            if (BuilderHelper.isDebugging()) {
                ErlLogger.trace(
                        "build",
                        " Done " + project.getName() + " took "
                                + Long.toString(System.currentTimeMillis() - time));
            }
        }
        return null;
    }

    @Override
    public void clean(final IErlProject erlProject, final BuildNotifier notifier) {
        final IProject currentProject = erlProject.getWorkspaceProject();
        if (currentProject == null || !currentProject.isAccessible()) {
            return;
        }

        if (BuilderHelper.isDebugging()) {
            ErlLogger.trace("build", "Cleaning " + currentProject.getName() //$NON-NLS-1$
                    + " @ " + new Date(System.currentTimeMillis()));
        }

        try {
            initializeBuilder(notifier);
            MarkerUtils.removeProblemMarkersFor(currentProject);
            final IFolder bf = currentProject.getFolder(erlProject.getProperties()
                    .getOutputDir());
            if (bf.exists()) {
                cleanupOutput(bf, notifier);
            }

        } catch (final Exception e) {
            ErlLogger.error(e);
            final String msg = NLS.bind(BuilderMessages.build_inconsistentProject,
                    e.getLocalizedMessage(), e.getClass().getName());
            MarkerUtils.createProblemMarker(currentProject, null, msg, 0,
                    IMarker.SEVERITY_ERROR);
        } finally {
            cleanup(notifier);
            if (BuilderHelper.isDebugging()) {
                ErlLogger.debug("Finished cleaning " + currentProject.getName() //$NON-NLS-1$
                        + " @ " + new Date(System.currentTimeMillis()));
            }
        }
    }

    private void cleanupOutput(final IFolder folder, final BuildNotifier notifier)
            throws CoreException {
        final IResource[] beams = folder.members();
        notifier.beginTask("Cleaning Erlang files", beams.length);
        if (beams.length > 0) {
            final float ndelta = 100.0f / beams.length;
            for (final IResource element : beams) {
                if ("beam".equals(element.getFileExtension())) {
                    final IResource source = findCorrespondingSource(element);
                    if (source != null) {
                        element.delete(true, notifier.monitor);
                    }
                    notifier.updateProgressDelta(ndelta);
                }
                if ("app".equals(element.getFileExtension())) {
                    final IResource source = findCorrespondingSource(element);
                    if (source != null) {
                        element.delete(true, notifier.monitor);
                    }
                }
            }
        }
    }

    private void handleErlangFiles(final IErlProject erlProject,
            final @NonNull IProject project, final BuildKind kind,
            final IResourceDelta resourceDelta, final BuildNotifier notifier)
            throws CoreException, BackendException {
        final OtpErlangList compilerOptions = CompilerOptions.get(project);

        final Set<BuildResource> resourcesToBuild = getResourcesToBuild(kind, project,
                resourceDelta, notifier);
        final int n = resourcesToBuild.size();
        if (n == 0) {
            return;
        }
        if (erlProject == null) {
            return;
        }
        // if (BuilderHelper.isDebugging()) {
        ErlLogger.debug("Will compile %d resource(s)", Integer.valueOf(n));
        // }
        final IBackend backend = BackendCore.getBackendManager().getBuildBackend(
                erlProject);
        if (backend == null) {
            final String message = "No backend with the required "
                    + "version could be found. Can't build.";
            MarkerUtils.createProblemMarker(project, null, message, 0,
                    IMarker.SEVERITY_ERROR);
            throw new BackendException(message);
        }
        final IErlModel model = ErlangEngine.getInstance().getModel();
        backend.addProjectPath(model.findProject(project));

        notifier.setProgressPerCompilationUnit(1.0f / n);
        final Map<RpcFuture, IResource> results = new HashMap<RpcFuture, IResource>();
        for (final BuildResource bres : resourcesToBuild) {
            notifier.checkCancel();
            final IResource resource = bres.getResource();
            MarkerUtils.deleteMarkers(resource);
            notifier.aboutToCompile(resource);
            if ("erl".equals(resource.getFileExtension())) {
                final String outputDir = erlProject.getProperties().getOutputDir()
                        .toString();
                final RpcFuture f = helper.startCompileErl(project, bres, outputDir,
                        backend.getOtpRpc(), compilerOptions, kind == BuildKind.FULL);
                if (f != null) {
                    results.put(f, resource);
                }
            } else if ("yrl".equals(resource.getFileExtension())) {
                final RpcFuture f = helper.startCompileYrl(project, resource,
                        backend.getOtpRpc(), compilerOptions);
                if (f != null) {
                    results.put(f, resource);
                }
            } else {
                ErlLogger.warn("Don't know how to compile: %s", resource.getName());
            }
        }

        final List<Entry<RpcFuture, IResource>> done = Lists.newArrayList();
        final List<Entry<RpcFuture, IResource>> waiting = Lists.newArrayList(results
                .entrySet());

        // TODO should use some kind of notification!
        while (!waiting.isEmpty()) {
            for (final Entry<RpcFuture, IResource> result : waiting) {
                notifier.checkCancel();
                OtpErlangObject r;
                try {
                    r = result.getKey().get(100, TimeUnit.MILLISECONDS);
                } catch (final Exception e) {
                    r = null;
                }
                if (r != null) {
                    final IResource resource = result.getValue();

                    helper.completeCompile(project, resource, r, backend.getOtpRpc(),
                            compilerOptions);
                    notifier.compiled(resource);

                    done.add(result);
                }
            }
            waiting.removeAll(done);
            done.clear();
        }
        helper.refreshOutputDir(project);

        try {
            helper.checkForClashes(backend.getOtpRpc(), project);
        } catch (final Exception e) {
        }
        backend.removeProjectPath(model.findProject(project));

    }

    private void handleAppFile(final IProject project, final String outPath,
            final Collection<IPath> sources) {
        if (SystemConfiguration.hasFeatureEnabled("erlide.no_app_src")) {
            return;
        }
        // if project doesn't look like an OTP app, skip this step
        if (!sources.contains(new Path("src"))) {
            return;
        }

        // ignore other dirs than 'src'
        final IPath src = new Path("src");
        final IFolder dir = (IFolder) project.findMember(src);
        if (dir != null) {
            try {
                for (final IResource file : dir.members()) {
                    final String name = file.getName();
                    if (name.endsWith(".app.src")) {
                        final String appSrc = file.getLocation().toPortableString();
                        final String destPath = outPath + "/"
                                + name.substring(0, name.lastIndexOf('.'));
                        final Collection<String> modules = gatherModules(project);
                        fillAppFileDetails(project, appSrc, destPath, modules);
                    }
                }
            } catch (final CoreException e) {
                ErlLogger.error(e);
            }
        }
    }

    private Collection<String> gatherModules(final IProject project) {
        final Collection<String> modules = newArrayList();
        try {
            final IErlProject erlangProject = ErlangEngine.getInstance().getModel()
                    .getErlangProject(project);
            for (final IErlModule m : erlangProject.getModules()) {
                // ignore rebar deps;
                if (!ignoreModule(erlangProject, m)) {
                    modules.add(m.getModuleName());
                }
            }
        } catch (final ErlModelException e1) {
            ErlLogger.error(e1);
        }
        return modules;
    }

    private boolean ignoreModule(final IErlProject erlangProject, final IErlModule m) {
        boolean result = false;
        result |= m.getSourceKind() != SourceKind.ERL;
        result |= !isModuleOnDirectSourcePath(erlangProject, m);
        result |= m.getResource().getProjectRelativePath().segment(0).equals("deps");
        if (result) {
            ErlLogger.debug(".app: ignore " + m.getResource().getProjectRelativePath());
        }
        return result;
    }

    private boolean isModuleOnDirectSourcePath(final IErlProject erlangProject,
            final IErlModule m) {
        boolean result = false;
        final List<IPath> sourceDirs = Lists.newArrayList(erlangProject.getProperties()
                .getSourceDirs());

        for (final IPath p : sourceDirs) {
            if (m.getResource().getParent().getProjectRelativePath().equals(p)) {
                result = true;
                break;
            }
        }
        return result;
    }

    private void fillAppFileDetails(final IProject project, final String appSrc,
            final String destPath, final Collection<String> modules) {
        try {
            final IErlProject eproject = ErlangEngine.getInstance().getModel()
                    .findProject(project);
            if (eproject == null) {
                return;
            }
            final IBackend backend = BackendCore.getBackendManager().getBuildBackend(
                    eproject);
            backend.getOtpRpc().call("erlide_builder", "compile_app_src", "ssla",
                    appSrc, destPath, modules);
        } catch (final Exception e) {
            ErlLogger.error(e);
        }
    }

    private void initializeBuilder(final BuildNotifier notifier) {
        notifier.begin();
    }

    private void cleanup(final BuildNotifier notifier) {
        notifier.done();
    }

    private Set<BuildResource> getResourcesToBuild(final BuildKind kind,
            final IProject currentProject, final IResourceDelta myDelta,
            final BuildNotifier notifier) throws CoreException {
        Set<BuildResource> resourcesToBuild = Sets.newHashSet();
        notifier.beginTask("retrieving resources to build", IProgressMonitor.UNKNOWN);
        if (kind == BuildKind.FULL) {
            resourcesToBuild = helper.getAffectedResources(currentProject, notifier);
        } else {
            final Path path = new Path(".settings/org.erlide.core.prefs");
            if (myDelta != null && myDelta.findMember(path) != null) {
                ErlLogger.info("project configuration changed: doing full rebuild");
                resourcesToBuild = helper.getAffectedResources(currentProject, notifier);
            } else {
                resourcesToBuild = helper.getAffectedResources(myDelta, notifier);
            }
        }
        notifier.doneTask();
        return resourcesToBuild;
    }

    public IResource findCorrespondingSource(final IResource beam) throws CoreException {
        final String[] p = beam.getName().split("\\.");
        final SearchVisitor searcher = helper.new SearchVisitor(p[0], null);
        beam.getProject().accept(searcher);
        final IResource source = searcher.getResult();
        return source;
    }

    @Override
    public BuilderProperties getProperties() {
        return null;
    }

}
