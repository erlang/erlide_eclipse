package org.erlide.dialyzer.internal.builder;

import java.lang.reflect.InvocationTargetException;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.osgi.util.NLS;
import org.erlide.backend.BackendCore;
import org.erlide.backend.api.IBackend;
import org.erlide.dialyzer.builder.DialyzerMarkerUtils;
import org.erlide.dialyzer.builder.DialyzerPreferences;
import org.erlide.dialyzer.builder.DialyzerUtils;
import org.erlide.dialyzer.builder.DialyzerUtils.DialyzerErrorException;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.util.ErlLogger;

import com.google.common.collect.Sets;

public class DialyzerBuilder extends IncrementalProjectBuilder {

    public static final String BUILDER_ID = "org.erlide.core.builder.dialyzer";

    // private static final BuilderHelper helper = new BuilderHelper();

    @Override
    protected IProject[] build(final int kind,
            @SuppressWarnings("rawtypes") final Map args, final IProgressMonitor monitor)
            throws CoreException {
        final IProject project = getProject();
        if (project == null) {
            monitor.done();
            return null;
        }
        DialyzerPreferences prefs = null;
        prefs = DialyzerPreferences.get(project);
        if (prefs == null || !prefs.getDialyzeOnCompile()) {
            return null;
        }
        final IErlElementLocator model = ErlangEngine.getInstance().getModel();
        final Set<IErlModule> modules = DialyzerUtils.collectModulesFromResource(model,
                project);
        final Set<IErlProject> projects = Sets.newHashSet();
        projects.add(model.findProject(project));
        if (!modules.isEmpty()) {
            try {
                final IErlProject eproject = model.findProject(project);
                if (eproject == null) {
                    return null;
                }
                final IBackend backend = BackendCore.getBackendManager().getBuildBackend(
                        eproject);
                DialyzerUtils.doDialyze(monitor, modules, projects, backend);
            } catch (final InvocationTargetException e) {
                ErlLogger.error(e);
            } catch (final DialyzerErrorException e) {
                ErlLogger.error(e);
                final String msg = NLS.bind(BuilderMessages.build_dialyzerProblem,
                        e.getLocalizedMessage());
                DialyzerMarkerUtils.addProblemMarker(project, null, null, msg, 0,
                        IMarker.SEVERITY_ERROR);
            }
        }
        monitor.done();
        return null;
    }

    @Override
    protected void clean(final IProgressMonitor monitor) throws CoreException {
        final IProject project = getProject();
        if (project == null || !project.isAccessible()) {
            return;
        }
        DialyzerMarkerUtils.removeDialyzerMarkersFor(project);
    }

}
