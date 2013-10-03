package org.erlide.core.internal.builder;

import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.erlide.core.builder.IBuilder;
import org.erlide.core.builder.MarkerUtils;
import org.erlide.core.internal.executor.ToolExecutor;
import org.erlide.core.internal.executor.ToolExecutor.ToolResults;
import org.erlide.util.ErlLogger;

import com.google.common.base.Joiner;

public abstract class ExternalBuilder implements IBuilder {

    protected final ToolExecutor ex;
    protected final String location;
    protected final IProject project;

    public ExternalBuilder(final IProject project, final String cmd) {
        this.project = project;
        ex = new ToolExecutor();
        location = ex.getToolLocation(cmd);
    }

    @Override
    public IProject[] build(final int kind, final Map<String, String> args,
            final IResourceDelta delta, final IProgressMonitor monitor) {
        final SubMonitor m = SubMonitor.convert(monitor, 10);

        MarkerUtils.removeProblemMarkersFor(project);
        m.worked(1);

        final ToolResults result = ex.run(location, getCompileTarget(), project
                .getLocation().toPortableString());
        createMarkers(result);
        m.worked(9);
        return null;
    }

    @Override
    public void clean(final IProgressMonitor monitor) {
        final SubMonitor m = SubMonitor.convert(monitor, 10);

        MarkerUtils.removeProblemMarkersFor(project);
        m.worked(1);

        if (getCleanTarget() == null) {
            return;
        }
        final ToolResults result = ex.run(location, getCleanTarget(), project
                .getLocation().toPortableString());
        if (result != null) {
            createMarkers(result);
        }
        m.worked(9);
    }

    protected String getCompileTarget() {
        return "compile";
    }

    protected String getCleanTarget() {
        return "clean";
    }

    private void createMarkers(final ToolResults result) {
        if (result.exit > 2) {
            ErlLogger.error("The '" + location + "' builder returned error "
                    + result.exit + "\n" + Joiner.on('\n').join(result.output)
                    + "--------------\n" + Joiner.on('\n').join(result.error)
                    + "--------------");
            return;
        }

        final IMessageParser parser = getMessageParser();
        for (final String msg : result.output) {
            parser.createMarkers(msg);
        }
    }

    protected IMessageParser getMessageParser() {
        return new ErlcMessageParser(project);
    }

}
