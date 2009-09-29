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

import java.util.Date;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.osgi.util.NLS;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.builder.internal.BuildNotifier;
import org.erlide.core.builder.internal.BuilderMessages;
import org.erlide.core.builder.internal.MarkerGenerator;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.preferences.OldErlangProjectProperties;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.util.ErlLogger;
import org.osgi.service.prefs.BackingStoreException;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;


public class ErlangBuilder extends IncrementalProjectBuilder {

	IProject currentProject;
	IWorkspaceRoot workspaceRoot;
	BuildNotifier notifier;

	@Override
	protected void clean(final IProgressMonitor monitor) throws CoreException {
		currentProject = getProject();
		if (currentProject == null || !currentProject.isAccessible()) {
			return;
		}

		if (BuilderUtils.isDebugging()) {
			ErlLogger.debug("Cleaning " + currentProject.getName() //$NON-NLS-1$
					+ " @ " + new Date(System.currentTimeMillis()));
		}
		super.clean(monitor);

		notifier = new BuildNotifier(monitor, currentProject);
		notifier.begin();
		try {
			notifier.checkCancel();

			MarkerGenerator.deleteMarkers(currentProject);
			initializeBuilder();
			MarkerGenerator.removeProblemsAndTasksFor(currentProject);

			final OldErlangProjectProperties prefs = ErlangCore
					.getProjectProperties(currentProject);
			final IFolder bf = currentProject.getFolder(prefs.getOutputDir());
			if (bf.exists()) {
				final IResource[] beams = bf.members();
				monitor.beginTask("Cleaning Erlang files", beams.length);
				for (final IResource element : beams) {
					if ("beam".equals(element.getFileExtension())) {
						element.delete(true, monitor);
						monitor.worked(1);
					}
				}
			}

		} catch (final CoreException e) {
			ErlLogger.error(e);
			String msg = NLS.bind(BuilderMessages.build_inconsistentProject, e
					.getLocalizedMessage());
			MarkerGenerator.addProblemMarker(currentProject, null, msg, 0,
					IMarker.SEVERITY_ERROR);
		} catch (final BackendException e) {
			ErlLogger.error(e);
			String msg = NLS.bind(BuilderMessages.build_inconsistentProject, e
					.getLocalizedMessage());
			MarkerGenerator.addProblemMarker(currentProject, null, msg, 0,
					IMarker.SEVERITY_ERROR);
		} finally {
			notifier.done();
			cleanup();
		}
		if (BuilderUtils.isDebugging()) {
			ErlLogger.debug("Finished cleaning " + currentProject.getName() //$NON-NLS-1$
					+ " @ " + new Date(System.currentTimeMillis()));
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	protected IProject[] build(final int kind, final Map args,
			final IProgressMonitor monitor) throws CoreException {
		currentProject = getProject();
		if (currentProject == null || !currentProject.isAccessible()) {
			return new IProject[0];
		}

		if (BuilderUtils.isDebugging()) {
			ErlLogger.debug("Starting build " + BuilderUtils.buildKind(kind) + " of "
					+ currentProject.getName() + " @ "
					+ new Date(System.currentTimeMillis()));
		}
		BuildNotifier.resetProblemCounters();
		notifier = new BuildNotifier(monitor, currentProject);
		notifier.begin();
		try {
			MarkerGenerator.deleteMarkers(currentProject);
			initializeBuilder();

			CompilerPreferences prefs = new CompilerPreferences(currentProject);
			try {
				prefs.load();
			} catch (BackingStoreException e1) {
				e1.printStackTrace();
				throw new CoreException(new Status(IStatus.ERROR,
						ErlangPlugin.PLUGIN_ID,
						"could not retrieve compiler options"));
			}
			OtpErlangList compilerOptions = prefs.export();

			ErlLogger.debug("******** building %s: %s", getProject().getName(),
					compilerOptions);

			Set<IResource> resourcesToBuild = new HashSet<IResource>();
			IProgressMonitor submon = new NullProgressMonitor();
			// new SubProgressMonitor(monitor, 10);
			submon.beginTask("retrieving resources to build",
					IProgressMonitor.UNKNOWN);
			if (kind == FULL_BUILD) {
				resourcesToBuild = BuilderUtils.getAffectedResources(args,
						getProject(), submon);
			} else {
				final IResourceDelta delta = getDelta(currentProject);
				final Path path = new Path(".settings/org.erlide.core.prefs");
				if (delta.findMember(path) != null) {
					ErlLogger
							.info("project configuration changed: doing full rebuild");
					resourcesToBuild = BuilderUtils.getAffectedResources(args,
							getProject(), submon);
				} else {
					resourcesToBuild = BuilderUtils.getAffectedResources(args,
							delta, submon);
				}
			}
			submon.done();
			final int n = resourcesToBuild.size();
			if (BuilderUtils.isDebugging()) {
				ErlLogger.debug("Will compile %d resource(s): %s", Integer
						.valueOf(n), resourcesToBuild.toString());
			}
			if (n > 0) {
				final Backend backend = ErlangCore.getBackendManager()
						.getBuildBackend(currentProject);
				if (backend == null) {
					final String message = "No backend with the required "
							+ "version could be found. Can't build.";
					MarkerGenerator.addProblemMarker(currentProject, null,
							message, 0, IMarker.SEVERITY_ERROR);
					throw new BackendException(message);
				}

				notifier.setProgressPerCompilationUnit(1.0f / n);
				for (final IResource resource : resourcesToBuild) {
					// TODO call these in parallel - how to gather markers?
					notifier.aboutToCompile(resource);
					if ("erl".equals(resource.getFileExtension())) {
						BuilderUtils.compileFile(currentProject, resource, backend,
								compilerOptions);
					} else if ("yrl".equals(resource.getFileExtension())) {
						compileYrlFile(currentProject, resource, backend,
								compilerOptions);
					} else {
						ErlLogger.warn("Don't know how to compile: %s",
								resource.getName());
					}
					notifier.compiled(resource);
				}
				try {
					BuilderUtils.checkForClashes(backend, currentProject);
				} catch (final Exception e) {
				}
			}

		} catch (final CoreException e) {
			ErlLogger.error(e);
			String msg = NLS.bind(BuilderMessages.build_inconsistentProject, e
					.getLocalizedMessage());
			MarkerGenerator.addProblemMarker(currentProject, null, msg, 0,
					IMarker.SEVERITY_ERROR);
		} catch (final BackendException e) {
			ErlLogger.error(e);
		} finally {
			notifier.done();
			cleanup();
		}
		if (BuilderUtils.isDebugging()) {
			ErlLogger.debug("Finished build of " + currentProject.getName() //$NON-NLS-1$
					+ " @ " + new Date(System.currentTimeMillis()));
		}
		return null;
	}

	protected void compileYrlFile(final IProject project,
			final IResource resource, final Backend backend,
			OtpErlangList compilerOptions) {
		// final IPath projectPath = project.getLocation();
		// final OldErlangProjectProperties prefs = new
		// OldErlangProjectProperties(project);

		MarkerGenerator.deleteMarkers(resource);
		// try {
		// resource.deleteMarkers(PROBLEM_MARKER, true,
		// IResource.DEPTH_INFINITE);
		// } catch (final CoreException e1) {
		// }

		IPath erl = resource.getProjectRelativePath().removeFileExtension();
		erl = erl.addFileExtension("erl").setDevice(null);
		IResource br = project.findMember(erl);

		// TODO check timestamps!

		try {
			if (br != null) {
				br.delete(true, null);
			}

			OtpErlangObject r;

			final String input = resource.getLocation().toString();
			final String output = resource.getLocation().removeFileExtension()
					.toString();
			r = BuilderUtils.compileYrlFile(backend, input, output);

			if (r instanceof OtpErlangTuple) {
				// process compilation messages
				final OtpErlangTuple t = (OtpErlangTuple) r;
				final OtpErlangList l = (OtpErlangList) t.elementAt(1);
				MarkerGenerator.addErrorMarkers(resource, l);
			}

			resource.getParent().refreshLocal(IResource.DEPTH_ONE, null);
			br = project.findMember(erl);
			if (br != null) {
				br.setDerived(true);
				// br.touch() doesn't work...
				BuilderUtils.compileFile(project, br, backend, compilerOptions);
			}

		} catch (final Exception e) {
			e.printStackTrace();
		}

	}

	private void cleanup() {
		notifier = null;
	}

	private void initializeBuilder() throws CoreException, BackendException {
		workspaceRoot = currentProject.getWorkspace().getRoot();
	}

}
