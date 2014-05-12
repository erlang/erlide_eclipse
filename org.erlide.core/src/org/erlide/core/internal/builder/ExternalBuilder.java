package org.erlide.core.internal.builder;

import java.util.Map;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.erlide.core.builder.BuilderHelper;
import org.erlide.core.executor.ProgressCallback;
import org.erlide.core.executor.ToolExecutor;
import org.erlide.core.executor.ToolExecutor.ToolResults;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.builder.ErlangBuilder;
import org.erlide.engine.model.builder.MarkerUtils;
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

    public String getOsCommand() {
        return info.getOsCommand();
    }

    @Override
    public IProject[] build(final int kind, final Map<String, String> args,
            final IProgressMonitor monitor) throws CoreException {
        super.build(kind, args, monitor);

        final SubMonitor m = SubMonitor.convert(monitor, 10);
        final IProject project = getProject();

        ErlLogger.trace("build",
                "Start " + helper.buildKind(kind) + " for " + project.getName() + ": "
                        + getOsCommand());

        try {
            MarkerUtils.removeProblemMarkersFor(project);
            m.worked(1);

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
                    final IMessageParser parser = getMessageParser();
                    parser.createMarkers(line);
                }

                @Override
                public void stderr(final String line) {
                    if (DEBUG) {
                        System.out.println("??? " + line);
                    }
                }
            };
            final ToolResults result = ex.run(getOsCommand(), getCompileTarget(), project
                    .getLocation().toPortableString(), callback, m);

            if (result == null || result.isCommandNotFound()) {
                MarkerUtils.createProblemMarker(project, null,
                        "Builder command not found: " + getOsCommand(), 0,
                        IMarker.SEVERITY_ERROR);
            } else {
                final boolean noMarkersOnProject = project.findMarkers(IMarker.PROBLEM,
                        true, IResource.DEPTH_INFINITE).length == 0;
                if (noMarkersOnProject && result.exit > 0) {
                    MarkerUtils.createProblemMarker(project, null, "Builder error: "
                            + getOsCommand(), 0, IMarker.SEVERITY_ERROR);
                }
            }

            m.worked(9);

            ErlLogger.trace("build", "Done " + project.getName());

        } catch (final Error e) {
            e.printStackTrace();
            throw new CoreException(new Status(IStatus.ERROR, "org.erlide.core",
                    "builder error", e));
        }
        monitor.done();
        return null;
    }

    @Override
    public void clean(final IProgressMonitor monitor) {
        final SubMonitor m = SubMonitor.convert(monitor, 10);
        final IProject project = getProject();

        MarkerUtils.removeProblemMarkersFor(project);
        m.worked(1);

        if (getCleanTarget() == null) {
            return;
        }
        final ProgressCallback callback = new ProgressCallback() {

            @Override
            public void stdout(final String line) {
                final IMessageParser parser = getMessageParser();
                parser.createMarkers(line);
            }

            @Override
            public void stderr(final String line) {
            }
        };
        ex.run(getOsCommand(), getCleanTarget(),
                project.getLocation().toPortableString(), callback, m);
        m.worked(9);
    }

    protected IMessageParser getMessageParser() {
        return new ErlcMessageParser(getProject());
    }

    protected String getCompileTarget() {
        return "compile";
    }

    protected String getCleanTarget() {
        return "clean";
    }

}
