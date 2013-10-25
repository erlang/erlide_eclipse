package org.erlide.core.internal.builder;

import java.util.Collection;
import java.util.Map;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.erlide.core.builder.BuilderHelper;
import org.erlide.core.builder.MarkerUtils;
import org.erlide.core.executor.ToolExecutor;
import org.erlide.core.executor.ToolExecutor.ToolResults;
import org.erlide.engine.model.builder.ErlangBuilder;
import org.erlide.util.ErlLogger;

import com.google.common.base.Joiner;

public abstract class ExternalBuilder extends ErlangBuilder {

    protected final ToolExecutor ex;
    protected final BuilderHelper helper = new BuilderHelper();

    public ExternalBuilder() {
        ex = new ToolExecutor();
    }

    public abstract String getOsCommand();

    @Override
    public IProject[] build(final int kind, final Map<String, String> args,
            final IProgressMonitor monitor) throws CoreException {
        final SubMonitor m = SubMonitor.convert(monitor, 10);
        final IProject project = getProject();

        ErlLogger.trace("build",
                "Start " + helper.buildKind(kind) + " for " + project.getName() + ": "
                        + getOsCommand());

        MarkerUtils.removeProblemMarkersFor(project);
        m.worked(1);

        // TODO use project config!
        // XXX how do we know what make uses?
        final IResource ebin = project.findMember("ebin");
        if (ebin == null) {
            project.getFolder("ebin").create(true, true, null);
        }

        final ToolResults result = ex.run(getOsCommand(), getCompileTarget(), project
                .getLocation().toPortableString());

        if (result.isCommandNotFound()) {
            MarkerUtils.addMarker(null, project, null, "Builder command not found: "
                    + getOsCommand(), 0, IMarker.SEVERITY_ERROR,
                    MarkerUtils.PROBLEM_MARKER);
        } else {
            createMarkers(result);
        }
        m.worked(9);

        ErlLogger.trace("build", "Done " + project.getName());

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
        final ToolResults result = ex.run(getOsCommand(), getCleanTarget(), project
                .getLocation().toPortableString());
        if (result != null) {
            createMarkers(result);
        }
        m.worked(9);
    }

    private void createMarkers(final ToolResults result) {
        if (result.exit > 2) {
            ErlLogger.error("The '" + getOsCommand() + "' builder returned error "
                    + result.exit + "\n" + Joiner.on('\n').join(result.output)
                    + "--------------\n" + Joiner.on('\n').join(result.error)
                    + "--------------");
            return;
        }

        boolean markers = false;
        final IMessageParser parser = getMessageParser();
        for (final String msg : result.output) {
            markers |= parser.createMarkers(msg);
        }
        if (!markers && result.exit != 0) {
            MarkerUtils.addMarker(null, getProject(), null, "Build error: "
                    + strip(result.output), 1, IMarker.SEVERITY_ERROR, IMarker.PROBLEM);
        }
    }

    private String strip(final Collection<String> output) {
        String result = Joiner.on(' ').join(output);
        result = result.replaceAll(" +", " ");
        return result;
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
