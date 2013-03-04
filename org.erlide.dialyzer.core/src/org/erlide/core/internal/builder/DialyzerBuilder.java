package org.erlide.core.internal.builder;

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
import org.erlide.backend.BackendException;
import org.erlide.backend.IBackend;
import org.erlide.core.builder.DialyzerMarkerUtils;
import org.erlide.core.builder.DialyzerPreferences;
import org.erlide.core.builder.DialyzerUtils;
import org.erlide.core.builder.DialyzerUtils.DialyzerErrorException;
import org.erlide.model.erlang.IErlModule;
import org.erlide.model.root.ErlModelManager;
import org.erlide.model.root.IErlElementLocator;
import org.erlide.model.root.IErlProject;
import org.erlide.runtime.rpc.RpcException;

import com.google.common.collect.Sets;
import org.erlide.util.ErlLogger;

public class DialyzerBuilder extends IncrementalProjectBuilder {

    public static final String BUILDER_ID = "org.erlide.core.builder.dialyzer";

    // private static final BuilderHelper helper = new BuilderHelper();

    @Override
    protected IProject[] build(final int kind,
            @SuppressWarnings("rawtypes") final Map args,
            final IProgressMonitor monitor) throws CoreException {
        final IProject project = getProject();
        DialyzerPreferences prefs = null;
        try {
            prefs = DialyzerPreferences.get(project);
        } catch (final RpcException e1) {
            ErlLogger.warn(e1);
            return null;
        }
        if (prefs == null || !prefs.getDialyzeOnCompile()) {
            return null;
        }
        final IErlElementLocator model = ErlModelManager.getErlangModel();
        final Set<IErlModule> modules = DialyzerUtils.collectModulesFromResource(model, project);
        final Set<IErlProject> projects = Sets.newHashSet();
        projects.add(model.findProject(project));
        if (modules.size() != 0) {
            try {
            	final IBackend backend = BackendCore.getBackendManager().getBuildBackend(project);
                DialyzerUtils.doDialyze(monitor, modules, projects, backend);
            } catch (final BackendException e) {
            	ErlLogger.error(e);
            } catch (final InvocationTargetException e) {
                ErlLogger.error(e);
            } catch (final DialyzerErrorException e) {
            	ErlLogger.error(e);
            	final String msg = NLS.bind(
                        BuilderMessages.build_dialyzerProblem, e.getLocalizedMessage());
                DialyzerMarkerUtils.addProblemMarker(project, null, null, msg,
                        0, IMarker.SEVERITY_ERROR);
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
