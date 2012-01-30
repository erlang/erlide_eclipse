package org.erlide.core.internal.services.builder;

import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.osgi.util.NLS;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.root.ErlModelManager;
import org.erlide.core.model.root.IErlElementLocator;
import org.erlide.core.model.root.IErlProject;
import org.erlide.core.services.builder.BuilderMessages;
import org.erlide.core.services.builder.DialyzerPreferences;
import org.erlide.core.services.builder.DialyzerUtils;
import org.erlide.core.services.builder.MarkerUtils;
import org.erlide.jinterface.ErlLogger;
import org.erlide.jinterface.rpc.RpcException;

public class DialyzerBuilder extends IncrementalProjectBuilder {

    public static final String BUILDER_ID = "org.erlide.core.builder.dialyzer";

    // private static final BuilderHelper helper = new BuilderHelper();

    @Override
    protected IProject[] build(final int kind,
            @SuppressWarnings("rawtypes") final Map args,
            final IProgressMonitor monitor) throws CoreException {
        final IProject project = getProject();
        DialyzerPreferences prefs;
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
        final Map<IErlProject, Set<IErlModule>> modules = new HashMap<IErlProject, Set<IErlModule>>();
        DialyzerUtils.addModulesFromResource(model, project, modules);
        if (modules.size() != 0) {
            try {
                DialyzerUtils.doDialyze(monitor, modules);
            } catch (final InvocationTargetException e) {
                ErlLogger.error(e);
                final String msg = NLS.bind(
                        BuilderMessages.build_dialyzerProblem, e
                                .getTargetException().getLocalizedMessage());
                MarkerUtils.addProblemMarker(project, null, null, msg, 0,
                        IMarker.SEVERITY_ERROR);
            }
        }
        return null;
    }

    @Override
    protected void clean(final IProgressMonitor monitor) throws CoreException {
        final IProject project = getProject();
        if (project == null || !project.isAccessible()) {
            return;
        }
        MarkerUtils.removeDialyzerMarkers(project);
    }

}
