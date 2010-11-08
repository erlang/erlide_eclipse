/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution.
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.builder;

import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.Map;
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
import org.eclipse.core.runtime.Path;
import org.eclipse.osgi.util.NLS;
import org.erlide.core.builder.internal.BuildNotifier;
import org.erlide.core.builder.internal.BuilderMessages;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IOldErlangProjectProperties;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.rpc.EventHandler;
import org.erlide.jinterface.rpc.EventProcessor;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.collect.Sets;

public class ErlangBuilder2 extends IncrementalProjectBuilder {

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
        final IProject project = getProject();
        if (project == null || !project.isAccessible()) {
            return new IProject[0];
        }

        if (BuilderHelper.isDebugging()) {
            ErlLogger.debug("Starting build " + helper.buildKind(kind) + " of "
                    + project.getName() + " @ "
                    + new Date(System.currentTimeMillis()));
        }
        try {
            MarkerUtils.deleteMarkers(project);
            initializeBuilder(monitor);

            final OtpErlangList compilerOptions = CompilerPreferences
                    .get(project);

            ErlLogger.debug("******** building %s: %s", getProject().getName(),
                    compilerOptions);

            final Set<String> resourcesToBuild = getResourcesToBuild(kind,
                    args, project);
            final int n = resourcesToBuild.size();
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

                final IPath projectPath = project.getLocation();
                final IOldErlangProjectProperties prefs = ErlangCore
                        .getProjectProperties(project);
                final String outputDir = projectPath.append(
                        prefs.getOutputDir()).toString();
                helper.ensureDirExists(outputDir);

                final Collection<IPath> includeDirs = helper
                        .getAllIncludeDirs(project);

                final EventProcessor processor = new EventProcessor(
                        new BuildHandler(), backend);
                final OtpErlangPid watcher = processor.getPid();
                final OtpErlangPid builder = (OtpErlangPid) backend.call(
                        "erlide_builder", "build_resources", "lsslsxp",
                        resourcesToBuild, outputDir, includeDirs,
                        compilerOptions, watcher);
                processor.setStarter(builder);
                processor.run();
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
            if (BuilderHelper.isDebugging()) {
                ErlLogger.debug("Finished build of " + project.getName() //$NON-NLS-1$
                        + " @ " + new Date(System.currentTimeMillis()));
            }
        }
        return null;
    }

    private Set<String> getResourcesToBuild(final int kind,
            @SuppressWarnings("rawtypes") final Map args, final IProject project)
            throws CoreException {
        Set<BuildResource> result = Sets.newHashSet();
        final IProgressMonitor submon = new NullProgressMonitor();
        // new SubProgressMonitor(monitor, 10);
        submon.beginTask("retrieving resources to build",
                IProgressMonitor.UNKNOWN);
        if (kind == FULL_BUILD) {
            result = helper.getAffectedResources(args, project, submon);
        } else {
            final IResourceDelta delta = getDelta(project);
            final Path path = new Path(".settings/org.erlide.core.prefs");
            if (delta.findMember(path) != null) {
                ErlLogger
                        .info("project configuration changed: doing full rebuild");
                result = helper.getAffectedResources(args, project, submon);
            } else {
                result = helper.getAffectedResources(args, delta, submon);
            }
        }
        if (BuilderHelper.isDebugging()) {
            ErlLogger.debug("Will compile %d resource(s): %s",
                    Integer.valueOf(result.size()), result.toString());
        }
        submon.done();
        final Set<String> paths = new HashSet<String>();
        for (final BuildResource res : result) {
            paths.add(res.getResource().getLocation().toPortableString());
        }
        return paths;
    }

    private void initializeBuilder(final IProgressMonitor monitor)
            throws CoreException, BackendException {
        final IProject currentProject = getProject();
        notifier = new BuildNotifier(monitor, currentProject);
        notifier.begin();
    }

    private void cleanup() {
        notifier.done();
        notifier = null;
    }

    class BuildHandler implements EventHandler {

        public boolean handleEvent(final OtpErlangObject msg) {
            notifier.checkCancel();

            ErlLogger.debug(">>> %s", msg);
            if (msg instanceof OtpErlangTuple) {
                try {
                    final OtpErlangTuple tuple = (OtpErlangTuple) msg;
                    final OtpErlangAtom akey = (OtpErlangAtom) tuple
                            .elementAt(0);
                    final String key = akey.atomValue();
                    final OtpErlangObject value = tuple.elementAt(1);

                    if ("compile".equals(key)) {
                        // value = {ok, Messages, Files} | {error, Messages}
                        // TODO create markers for messages
                        // TODO refresh Files and mark as derived
                    }
                    if ("tasks".equals(key)) {
                        // value = [Task]
                        // TODO create tasks for Tasks
                    }
                    if ("clash".equals(key)) {
                        // value = [Clash]
                        // TODO create markers for clashes
                    }
                } catch (final Exception e) {
                    // ignore?
                }
            }
            return msg.equals(new OtpErlangAtom("stop"));
        }
    }

}
