package org.erlide.core.internal.builder;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.erlide.core.ErlangCore;
import org.erlide.core.builder.BuilderHelper;
import org.erlide.core.executor.ProgressCallback;
import org.erlide.core.executor.ToolExecutor;
import org.erlide.core.executor.ToolExecutor.ToolResults;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.builder.MarkerUtils;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.util.ErlLogger;

public abstract class ExternalBuilder extends ErlangBuilder {

    protected static final boolean DEBUG = false;

    protected final ToolExecutor ex;
    protected final BuilderHelper helper = new BuilderHelper();
    protected final BuilderTool info;

    public ExternalBuilder(final BuilderTool info) {
        this.info = info;
        ex = new ToolExecutor();
    }

    public String getOsCommand(final IErlProject erlProject) {
        return info.getOsCommand();
    }

    @Override
    public IProject[] build(final BuildKind kind, final IErlProject erlProject,
            final BuildNotifier notifier) throws CoreException {
        final IProject project = erlProject.getWorkspaceProject();

        ErlLogger.trace("build", "Start " + kind + " for " + project.getName() + ": "
                + getOsCommand(erlProject));

        try {
            MarkerUtils.removeProblemMarkersFor(project);
            notifier.worked(1);

            // TODO use project config!
            // XXX how do we know what make uses?
            final IResource ebin = project.findMember("ebin");
            if (ebin == null) {
                project.getFolder("ebin").create(true, true, null);
            }

            final ProgressCallback callback = new ProgressCallback() {

                @Override
                public void stdout(final String line) {
                    if (DEBUG) {
                        System.out.println("!!! " + line);
                    }
                    final IMessageParser parser = getMessageParser(erlProject);
                    parser.createMarkers(line);
                }

                @Override
                public void stderr(final String line) {
                    if (DEBUG) {
                        System.out.println("??? " + line);
                    }
                }
            };
            final ToolResults result = ex.run(getOsCommand(erlProject),
                    getCompileTarget(), project.getLocation().toPortableString(),
                    callback, notifier);

            if (result == null || result.isCommandNotFound()) {
                MarkerUtils.createProblemMarker(project, null,
                        "Builder command not found: " + getOsCommand(erlProject), 0,
                        IMarker.SEVERITY_ERROR);
            } else {
                final boolean noMarkersOnProject = project.findMarkers(IMarker.PROBLEM,
                        true, IResource.DEPTH_INFINITE).length == 0;
                if (noMarkersOnProject && result.exit > 0) {
                    MarkerUtils.createProblemMarker(project, null, "Builder error: "
                            + getOsCommand(erlProject), 0, IMarker.SEVERITY_ERROR);
                }
            }

            notifier.worked(9);

            ErlLogger.trace("build", "Done " + project.getName());

        } catch (final Error e) {
            e.printStackTrace();
            throw new CoreException(new Status(IStatus.ERROR, ErlangCore.PLUGIN_ID,
                    "builder error", e));
        }
        notifier.done();
        return null;
    }

    @Override
    public void clean(final IErlProject erlProject, final BuildNotifier notifier) {
        final IProject project = erlProject.getWorkspaceProject();

        MarkerUtils.removeProblemMarkersFor(project);
        notifier.worked(1);

        if (getCleanTarget() == null) {
            return;
        }
        final ProgressCallback callback = new ProgressCallback() {

            @Override
            public void stdout(final String line) {
                final IMessageParser parser = getMessageParser(erlProject);
                parser.createMarkers(line);
            }

            @Override
            public void stderr(final String line) {
            }
        };
        ex.run(getOsCommand(erlProject), getCleanTarget(), project.getLocation()
                .toPortableString(), callback, notifier);
        notifier.worked(9);
    }

    protected IMessageParser getMessageParser(final IErlProject erlProject) {
        return new ErlcMessageParser(erlProject.getWorkspaceProject());
    }

    protected String getCompileTarget() {
        return "compile";
    }

    protected String getCleanTarget() {
        return "clean";
    }

}
