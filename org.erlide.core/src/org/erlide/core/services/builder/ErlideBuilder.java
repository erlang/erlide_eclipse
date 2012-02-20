/*******************************************************************************
 * Copyright (c) 2009 * and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     *
 *******************************************************************************/
package org.erlide.core.services.builder;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.osgi.util.NLS;
import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendException;
import org.erlide.backend.IBackend;
import org.erlide.core.internal.model.root.OldErlangProjectProperties;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.services.builder.BuilderHelper.SearchVisitor;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.IRpcFuture;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public class ErlideBuilder {

    BuildNotifier notifier;
    private final BuilderHelper helper = new BuilderHelper();
    private final IProject myProject;

    public ErlideBuilder(final IProject prj) {
        myProject = prj;
    }

    public void clean(final IProgressMonitor monitor) throws CoreException {
        final IProject currentProject = getProject();
        if (currentProject == null || !currentProject.isAccessible()) {
            return;
        }

        if (BuilderHelper.isDebugging()) {
            ErlLogger.debug("Cleaning " + currentProject.getName() //$NON-NLS-1$
                    + " @ " + new Date(System.currentTimeMillis()));
        }

        try {
            initializeBuilder(monitor);
            MarkerUtils.removeProblemsAndTasksFor(currentProject);
            final IErlProject erlProject = ErlModelManager.getErlangModel()
                    .getErlangProject(currentProject);
            final IFolder bf = currentProject.getFolder(erlProject
                    .getOutputLocation());
            if (bf.exists()) {
                final boolean nukeOutput = new OldErlangProjectProperties(
                        currentProject).isNukeOutputOnClean();
                if (nukeOutput) {
                    bf.delete(true, monitor);
                } else {
                    final IResource[] beams = bf.members();
                    monitor.beginTask("Cleaning Erlang files", beams.length);
                    if (beams.length > 0) {
                        final float delta = 100.0f / beams.length;
                        for (final IResource element : beams) {
                            if ("beam".equals(element.getFileExtension())) {
                                final IResource source = findCorrespondingSource(element);
                                if (source != null) {
                                    element.delete(true, monitor);
                                }
                                notifier.updateProgressDelta(delta);
                            }
                        }
                    }
                }
            }

        } catch (final Exception e) {
            ErlLogger.error(e);
            final String msg = NLS.bind(
                    BuilderMessages.build_inconsistentProject,
                    e.getLocalizedMessage());
            MarkerUtils.addProblemMarker(currentProject, null, null, msg, 0,
                    IMarker.SEVERITY_ERROR);
        } finally {
            cleanup();
            if (BuilderHelper.isDebugging()) {
                ErlLogger.debug("Finished cleaning " + currentProject.getName() //$NON-NLS-1$
                        + " @ " + new Date(System.currentTimeMillis()));
            }
        }
    }

    public IProject[] build(final int kind,
            @SuppressWarnings("rawtypes") final Map args,
            final IProgressMonitor monitor, final IResourceDelta resourceDelta)
            throws CoreException {
        final long time = System.currentTimeMillis();
        final IProject project = getProject();
        if (project == null || !project.isAccessible()) {
            return new IProject[0];
        }

        // if (BuilderHelper.isDebugging()) {
        ErlLogger.debug("###** Starting build " + helper.buildKind(kind)
                + " of " + project.getName());
        // }
        final IErlProject erlProject = ErlModelManager.getErlangModel()
                .getErlangProject(project);
        try {
            MarkerUtils.deleteMarkers(project);
            initializeBuilder(monitor);

            final IPath out = erlProject.getOutputLocation();
            final IResource outr = project.findMember(out);
            if (outr != null) {
                try {
                    outr.setDerived(true, null);
                    outr.refreshLocal(IResource.DEPTH_ZERO, null);
                } catch (final CoreException e) {
                    // ignore it
                }
            }

            final OtpErlangList compilerOptions = CompilerOptions.get(project);
            ErlLogger.debug(">>> compiler options ::: " + compilerOptions);

            final Set<BuildResource> resourcesToBuild = getResourcesToBuild(
                    kind, args, project, resourceDelta);
            final int n = resourcesToBuild.size();
            // if (BuilderHelper.isDebugging()) {
            ErlLogger.debug("Will compile %d resource(s)", Integer.valueOf(n));
            // }
            if (n > 0) {
                final IBackend backend = BackendCore.getBackendManager()
                        .getBuildBackend(project);
                if (backend == null) {
                    final String message = "No backend with the required "
                            + "version could be found. Can't build.";
                    MarkerUtils.addProblemMarker(project, null, null, message,
                            0, IMarker.SEVERITY_ERROR);
                    throw new BackendException(message);
                }
                backend.addProjectPath(project);

                notifier.setProgressPerCompilationUnit(1.0f / n);
                final Map<IRpcFuture, IResource> results = new HashMap<IRpcFuture, IResource>();
                for (final BuildResource bres : resourcesToBuild) {
                    notifier.checkCancel();
                    final IResource resource = bres.getResource();
                    // notifier.aboutToCompile(resource);
                    if ("erl".equals(resource.getFileExtension())) {
                        final String outputDir = erlProject.getOutputLocation()
                                .toString();
                        final IRpcFuture f = helper.startCompileErl(project,
                                bres, outputDir, backend, compilerOptions,
                                kind == IncrementalProjectBuilder.FULL_BUILD);
                        if (f != null) {
                            results.put(f, resource);
                        }
                    } else if ("yrl".equals(resource.getFileExtension())) {
                        final IRpcFuture f = helper.startCompileYrl(project,
                                resource, backend, compilerOptions);
                        if (f != null) {
                            results.put(f, resource);
                        }
                    } else {
                        ErlLogger.warn("Don't know how to compile: %s",
                                resource.getName());
                    }
                }

                final List<Entry<IRpcFuture, IResource>> done = Lists
                        .newArrayList();
                final List<Entry<IRpcFuture, IResource>> waiting = Lists
                        .newArrayList(results.entrySet());

                // TODO should use some kind of notification!
                while (waiting.size() > 0) {
                    for (final Entry<IRpcFuture, IResource> result : waiting) {
                        notifier.checkCancel();
                        OtpErlangObject r;
                        try {
                            r = result.getKey().get(100);
                        } catch (final Exception e) {
                            r = null;
                        }
                        if (r != null) {
                            final IResource resource = result.getValue();

                            helper.completeCompile(project, resource, r,
                                    backend, compilerOptions);
                            notifier.compiled(resource);

                            done.add(result);
                        }
                    }
                    waiting.removeAll(done);
                    done.clear();
                }
                helper.refreshOutputDir(project);

                try {
                    helper.checkForClashes(backend, project);
                } catch (final Exception e) {
                }
                backend.removeProjectPath(project);
            }

        } catch (final OperationCanceledException e) {
            if (BuilderHelper.isDebugging()) {
                ErlLogger.debug("Build of " + project.getName()
                        + " was canceled.");
            }
        } catch (final Exception e) {
            ErlLogger.error(e);
            final String msg = NLS.bind(
                    BuilderMessages.build_inconsistentProject, e.getMessage(),
                    e.getClass().getName());
            MarkerUtils.addProblemMarker(project, null, null, msg, 0,
                    IMarker.SEVERITY_ERROR);
        } finally {
            cleanup();
            // if (BuilderHelper.isDebugging()) {
            ErlLogger.debug("###** Finished build of " + project.getName()
                    + " took "
                    + Long.toString(System.currentTimeMillis() - time));
            // }
        }
        return null;
    }

    private void initializeBuilder(final IProgressMonitor monitor) {
        final IProject currentProject = getProject();
        notifier = new BuildNotifier(monitor, currentProject);
        notifier.begin();
    }

    private void cleanup() {
        notifier.done();
        notifier = null;
    }

    private Set<BuildResource> getResourcesToBuild(final int kind,
            @SuppressWarnings("rawtypes") final Map args,
            final IProject currentProject, final IResourceDelta resourceDelta)
            throws CoreException {
        Set<BuildResource> resourcesToBuild = Sets.newHashSet();
        final IProgressMonitor submon = new SubProgressMonitor(
                notifier.fMonitor, 10);
        submon.beginTask("retrieving resources to build",
                IProgressMonitor.UNKNOWN);
        if (kind == IncrementalProjectBuilder.FULL_BUILD) {
            resourcesToBuild = helper.getAffectedResources(args,
                    currentProject, submon);
        } else {
            final IResourceDelta delta = resourceDelta;
            final Path path = new Path(".settings/org.erlide.core.prefs");
            if (delta != null && delta.findMember(path) != null) {
                ErlLogger
                        .info("project configuration changed: doing full rebuild");
                resourcesToBuild = helper.getAffectedResources(args,
                        currentProject, submon);
            } else {
                resourcesToBuild = helper.getAffectedResources(args, delta,
                        submon);
            }
        }
        submon.done();
        return resourcesToBuild;
    }

    public IProject getProject() {
        return myProject;
    }

    public IResource findCorrespondingSource(final IResource beam)
            throws CoreException {
        final String[] p = beam.getName().split("\\.");
        final SearchVisitor searcher = helper.new SearchVisitor(p[0], null);
        beam.getProject().accept(searcher);
        final IResource source = searcher.getResult();
        return source;
    }

}
