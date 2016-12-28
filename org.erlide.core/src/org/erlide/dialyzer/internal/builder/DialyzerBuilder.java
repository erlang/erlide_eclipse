package org.erlide.dialyzer.internal.builder;

import java.lang.reflect.InvocationTargetException;
import java.util.Set;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.osgi.util.NLS;
import org.erlide.backend.BackendCore;
import org.erlide.backend.api.IBackend;
import org.erlide.core.builder.BuildNotifier;
import org.erlide.dialyzer.builder.DialyzerMarkerUtils;
import org.erlide.dialyzer.builder.DialyzerPreferences;
import org.erlide.dialyzer.builder.DialyzerUtils;
import org.erlide.dialyzer.builder.DialyzerUtils.DialyzerErrorException;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.model.root.IErlModule;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.util.ErlLogger;

import com.google.common.collect.Sets;

public class DialyzerBuilder {
	public static final String BUILDER_ID = "org.erlide.core.builder.dialyzer";
	private IProject project;

	public DialyzerBuilder(IProject project) {
		this.project = project;
	}

	public void build(final BuildNotifier notifier) throws CoreException {
		if (project == null) {
			return;
		}
		DialyzerPreferences prefs = null;
		prefs = DialyzerPreferences.get(project);
		if (prefs == null || !prefs.getDialyzeOnCompile()) {
			return;
		}
		final IErlElementLocator model = ErlangEngine.getInstance().getModel();
		final Set<IErlModule> modules = DialyzerUtils.collectModulesFromResource(model, project);
		final Set<IErlProject> projects = Sets.newHashSet();
		projects.add(model.findProject(project));
		if (!modules.isEmpty()) {
			try {
				final IErlProject eproject = model.findProject(project);
				if (eproject == null) {
					return;
				}
				final IBackend backend = BackendCore.getBackendManager().getBuildBackend(eproject);
				DialyzerUtils.doDialyze(notifier.monitor, modules, projects, backend);
			} catch (final InvocationTargetException e) {
				ErlLogger.error(e);
			} catch (final DialyzerErrorException e) {
				ErlLogger.error(e);
				final String msg = NLS.bind(BuilderMessages.build_dialyzerProblem, e.getLocalizedMessage());
				DialyzerMarkerUtils.addProblemMarker(project, null, null, msg, 0, IMarker.SEVERITY_ERROR);
			}
		}
	}

	public void clean(final BuildNotifier notifier) {
		if (project == null || !project.isAccessible()) {
			return;
		}
		DialyzerMarkerUtils.removeDialyzerMarkersFor(project);
	}

}
