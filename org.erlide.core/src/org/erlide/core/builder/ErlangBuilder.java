/*******************************************************************************
 * Copyright (c) 2004 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.builder;

import java.util.ArrayList;
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
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.osgi.util.NLS;
import org.erlide.core.builder.internal.BuildNotifier;
import org.erlide.core.builder.internal.BuilderMessages;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IOldErlangProjectProperties;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.rpc.RpcFuture;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Sets;

public class ErlangBuilder extends IncrementalProjectBuilder {

    BuildNotifier notifier;
    private final BuilderHelper helper = new BuilderHelper();

    @Override
    protected void clean(final IProgressMonitor monitor) throws CoreException {
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

            final IOldErlangProjectProperties prefs = ErlangCore
                    .getProjectProperties(currentProject);
            final IFolder bf = currentProject.getFolder(prefs.getOutputDir());
            if (bf.exists()) {
                final IResource[] beams = bf.members();
                monitor.beginTask("Cleaning Erlang files", beams.length);
                if (beams.length > 0) {
                    final float delta = 1.0f / beams.length;
                    for (final IResource element : beams) {
                        if ("beam".equals(element.getFileExtension())) {
                            element.delete(true, monitor);
                            notifier.updateProgressDelta(delta);
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

    @Override
    protected IProject[] build(final int kind,
            @SuppressWarnings("rawtypes") final Map args,
            final IProgressMonitor monitor) throws CoreException {
        final long time = System.currentTimeMillis();
        final IProject project = getProject();
        if (project == null || !project.isAccessible()) {
            return new IProject[0];
        }

        if (BuilderHelper.isDebugging()) {
            ErlLogger.debug("Starting build " + helper.buildKind(kind) + " of "
                    + project.getName());
        }
        final IOldErlangProjectProperties prefs = ErlangCore
                .getProjectProperties(project);
        try {
            MarkerUtils.deleteMarkers(project);
            initializeBuilder(monitor);

            final IPath out = prefs.getOutputDir();
            final IResource outr = project.findMember(out);
            if (outr != null) {
                try {
                    outr.setDerived(true);
                    outr.refreshLocal(IResource.DEPTH_ZERO, null);
                } catch (final CoreException e) {
                    // ignore it
                }
            }

            final OtpErlangList compilerOptions = CompilerPreferences
                    .get(project);

            ErlLogger.debug("******** building %s: %s", getProject().getName(),
                    compilerOptions);

            final Set<BuildResource> resourcesToBuild = getResourcesToBuild(
                    kind, args, project);
            final int n = resourcesToBuild.size();
            if (BuilderHelper.isDebugging()) {
                ErlLogger.debug("Will compile %d resource(s): %s",
                        Integer.valueOf(n), resourcesToBuild.toString());
            }
            if (n > 0) {
                final Backend backend = ErlangCore.getBackendManager()
                        .getBuildBackend(project);
                if (backend == null) {
                    final String message = "No backend with the required "
                            + "version could be found. Can't build.";
                    MarkerUtils.addProblemMarker(project, null, null, message,
                            0, IMarker.SEVERITY_ERROR);
                    throw new BackendException(message);
                }

                notifier.setProgressPerCompilationUnit(1.0f / n);
                final Map<RpcFuture, IResource> results = new HashMap<RpcFuture, IResource>();
                for (final BuildResource bres : resourcesToBuild) {
                    notifier.checkCancel();
                    final IResource resource = bres.getResource();
                    // notifier.aboutToCompile(resource);
                    if ("erl".equals(resource.getFileExtension())) {
                        final String outputDir = prefs.getOutputDir()
                                .toString();
                        final RpcFuture f = helper.startCompileErl(project,
                                bres, outputDir, backend, compilerOptions,
                                kind == IncrementalProjectBuilder.FULL_BUILD);
                        if (f != null) {
                            results.put(f, resource);
                        }
                    } else if ("yrl".equals(resource.getFileExtension())) {
                        final RpcFuture f = helper.startCompileYrl(project,
                                resource, backend, compilerOptions);
                        if (f != null) {
                            results.put(f, resource);
                        }
                    } else {
                        ErlLogger.warn("Don't know how to compile: %s",
                                resource.getName());
                    }
                }

                final List<Entry<RpcFuture, IResource>> done = new ArrayList<Entry<RpcFuture, IResource>>();
                final List<Entry<RpcFuture, IResource>> waiting = new ArrayList<Entry<RpcFuture, IResource>>(
                        results.entrySet());

                // TODO should use some kind of notification!
                while (waiting.size() > 0) {
                    for (final Entry<RpcFuture, IResource> result : waiting) {
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
            }

        } catch (final OperationCanceledException e) {
            if (BuilderHelper.isDebugging()) {
                ErlLogger.debug("Build of " + project.getName()
                        + " was canceled.");
            }
        } catch (final Exception e) {
            ErlLogger.error(e);
            final String msg = NLS.bind(
                    BuilderMessages.build_inconsistentProject,
                    e.getLocalizedMessage());
            MarkerUtils.addProblemMarker(project, null, null, msg, 0,
                    IMarker.SEVERITY_ERROR);
        } finally {
            cleanup();
            // if (BuilderHelper.isDebugging()) {
            ErlLogger.debug("Finished build of " + project.getName() //$NON-NLS-1$
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
            final IProject currentProject) throws CoreException {
        Set<BuildResource> resourcesToBuild = Sets.newHashSet();
        final IProgressMonitor submon = new NullProgressMonitor();
        // new SubProgressMonitor(monitor, 10);
        submon.beginTask("retrieving resources to build",
                IProgressMonitor.UNKNOWN);
        if (kind == FULL_BUILD) {
            resourcesToBuild = helper.getAffectedResources(args,
                    currentProject, submon);
        } else {
            final IResourceDelta delta = getDelta(currentProject);
            final Path path = new Path(".settings/org.erlide.core.prefs");
            if (delta.findMember(path) != null) {
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

}
