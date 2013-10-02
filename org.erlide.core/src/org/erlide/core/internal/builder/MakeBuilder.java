package org.erlide.core.internal.builder;

import java.util.Collection;

import org.eclipse.core.resources.IProject;
import org.erlide.core.internal.executor.ToolExecutor.ToolResults;

public class MakeBuilder extends ExternalBuilder {

    MakeBuilder(final IProject project) {
        super(project, "make");
    }

    Collection<String> getMakefileTargets(final String dir) {
        final ToolResults make = ex
                .run("/bin/bash",
                        "-c \"make -rpn | sed -n -e '/^$/ { n ; /^[^ ]*:/p }' | grep -v ^.PHONY | cut -d : -f 1\"",
                        project.getLocation().toPortableString());
        return make.output;
    }

}
