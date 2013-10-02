package org.erlide.core.internal.builder;

import java.util.Collection;
import java.util.Map;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubMonitor;
import org.erlide.core.builder.IBuilder;
import org.erlide.core.internal.executor.ToolExecutor;
import org.erlide.core.internal.executor.ToolExecutor.ToolResults;
import org.erlide.util.ErlLogger;

public class MakeBuilder implements IBuilder {

    private final IProject project;
    private final String COMPILE_TARGET = "compile";
    private final String CLEAN_TARGET = "clean";
    private final ToolExecutor ex;
    private final String makeLocation;

    MakeBuilder(final IProject project) {
        this.project = project;
        ex = new ToolExecutor();
        makeLocation = ex.getToolLocation("make");
    }

    @Override
    public IProject[] build(final int kind, final Map<String, String> args,
            final IResourceDelta delta, final IProgressMonitor monitor) {
        ErlLogger.debug("MakeBuilder: not implemented yet");

        final SubMonitor m = SubMonitor.convert(monitor, 10);

        final ToolResults result = ex.run(makeLocation, COMPILE_TARGET, project
                .getLocation().toPortableString());
        createMarkers(result);
        m.worked(10);
        return null;
    }

    private void createMarkers(final ToolResults result) {
        // exit!=0 ?
        // no target?
        // other error?

        // else parse output
    }

    @Override
    public void clean(final IProgressMonitor monitor) {
        final SubMonitor m = SubMonitor.convert(monitor, 10);
        final ToolResults result = ex.run(makeLocation, CLEAN_TARGET, project
                .getLocation().toPortableString());
        createMarkers(result);
        m.worked(10);
    }

    private Collection<String> getMakefileTargets(final String dir) {
        final ToolResults make = ex
                .run("/bin/bash",
                        "-c \"make -rpn | sed -n -e '/^$/ { n ; /^[^ ]*:/p }' | grep -v ^.PHONY | cut -d : -f 1\"",
                        project.getLocation().toPortableString());
        return make.output;
    }

}
