package org.erlide.core.builder;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.osgi.util.NLS;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.builder.internal.BuildNotifier;
import org.erlide.core.builder.internal.BuilderMessages;
import org.erlide.core.builder.internal.MarkerHelper;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.preferences.OldErlangProjectProperties;
import org.erlide.jinterface.backend.Backend;
import org.erlide.jinterface.backend.BackendException;
import org.erlide.jinterface.rpc.RpcFuture;
import org.erlide.jinterface.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public class DialyzerUtils {

	public static final String DIALYZE_WARNING_MARKER = ErlangPlugin.PLUGIN_ID
			+ ".dialyzewarningmarker";

	private static final int MAX_PROGRESS = 1000000;

	private int errorCount;

	public void dialyze(final Map<IErlProject, Set<IErlModule>> modules) {
		final long time = System.currentTimeMillis();
		if (BuilderUtils.isDebugging()) {
			ErlLogger.debug("Starting dialyze of " + modules);
		}
		resetProblemCounters();
		try {
			final Set<IErlProject> projects = modules.keySet();
			for (final IErlProject p : projects) {
				final IProject project = p.getProject();
				project.deleteMarkers(DIALYZE_WARNING_MARKER, false, IResource.DEPTH_ZERO);
//			initializeBuilder(monitor);

			final int n = projects.size();
			if (BuilderUtils.isDebugging()) {
				ErlLogger.debug("Will dialyze %d resource(s): %s", n, p.getName());
			}
			if (n > 0) {
				final Backend backend = ErlangCore.getBackendManager()
						.getBuildBackend(project);
				if (backend == null) {
					final String message = "No backend with the required "
							+ "version could be found. Can't build.";
					MarkerHelper.addProblemMarker(project, null, message, 0,
							IMarker.SEVERITY_ERROR);
					throw new BackendException(message);
				}
				final Job job= new UIJob() 
				job.setPriority(Job.LONG);
				job.setUser(true);

				final IProgressService service= PlatformUI.getWorkbench().getProgressService();
				if (service != null) {
					service.schedule(job, 0, true);
				} else {
					job.schedule();
				}


				final int progressPerProject = MAX_PROGRESS / n;
				final IProgressMonitor monitor = new 
				notifier.setProgressPerCompilationUnit(1.0f / n);
				final Map<RpcFuture, IResource> results = new HashMap<RpcFuture, IResource>();
				for (final BuildResource bres : resourcesToBuild) {
					notifier.checkCancel();
					final IResource resource = bres.getResource();
					// notifier.aboutToCompile(resource);
					if ("erl".equals(resource.getFileExtension())) {
						final RpcFuture f = BuilderUtils.startCompileErl(
								project, bres, backend, compilerOptions, false);
						if (f != null) {
							results.put(f, resource);
						}
					} else if ("yrl".equals(resource.getFileExtension())) {
						final RpcFuture f = BuilderUtils.startCompileYrl(
								project, resource, backend, compilerOptions);
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

							BuilderUtils.completeCompile(project, resource, r,
									backend, compilerOptions);
							notifier.compiled(resource);

							done.add(result);
						}
					}
					waiting.removeAll(done);
					done.clear();
				}
				BuilderUtils.refreshOutputDir(project);

				try {
					BuilderUtils.checkForClashes(backend, project);
				} catch (final Exception e) {
				}
			}

		} catch (final OperationCanceledException e) {
			if (BuilderUtils.isDebugging()) {
				ErlLogger.debug("Build of " + project.getName()
						+ " was canceled.");
			}
		} catch (final Exception e) {
			ErlLogger.error(e);
			final String msg = NLS.bind(
					BuilderMessages.build_inconsistentProject, e
							.getLocalizedMessage());
			MarkerHelper.addProblemMarker(project, null, msg, 0,
					IMarker.SEVERITY_ERROR);
		} finally {
			cleanup();
			if (BuilderUtils.isDebugging()) {
				ErlLogger.debug("Finished build of " + project.getName() //$NON-NLS-1$
						+ " took "
						+ Long.toString(System.currentTimeMillis() - time));
			}
		}
		return null;
	}

	private void resetProblemCounters() {
		errorCount = 0;
	}

}
