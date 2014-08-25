package org.erlide.core.internal.builder;

import java.util.Map;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.builder.MarkerUtils;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.ProjectConfigType;
import org.erlide.util.ErlLogger;

public class ErlangEclipseBuilder extends IncrementalProjectBuilder {

    @Override
    public IProject[] build(final int kind, final Map<String, String> args,
            final IProgressMonitor monitor) throws CoreException {

        final IProject project = getProject();
        if (project == null || !project.isAccessible()) {
            return null;
        }
        final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                .getErlangProject(project);
        final ProjectConfigType config = erlProject.getConfigType();
        final BuilderTool tool = erlProject.getBuilderProperties().getBuilderTool();

        if (!validateBuildConfiguration(erlProject)) {
            ErlLogger.warn("Builder tool and config mismatch: " + tool + " " + config);
            monitor.setCanceled(true);
        }

        final ErlangBuilder builder = ErlangBuilderFactory.get(tool);
        if (builder != null) {
            final BuildNotifier notifier = new BuildNotifier(monitor, project);

            if (builder instanceof InternalBuilder) {
                // temporary hack; rebar builder will not need this
                ((InternalBuilder) builder).setDelta(getDelta(project));
            }
            builder.build(ErlangBuilder.BuildKind.get(kind), erlProject, notifier);
        }

        return null;
    }

    private boolean validateBuildConfiguration(final IErlProject erlProject) {
        final ProjectConfigType config = erlProject.getConfigType();
        final BuilderTool tool = erlProject.getBuilderProperties().getBuilderTool();
        if (!config.matchesTool(tool)) {
            final String msg = String.format(
                    "Project's builder tool %s and configuration %s don't match", tool,
                    config);
            MarkerUtils.createProblemMarker(erlProject.getWorkspaceProject(), null, msg,
                    0, IMarker.SEVERITY_WARNING);
            return false;
        }
        return true;
    }

}
