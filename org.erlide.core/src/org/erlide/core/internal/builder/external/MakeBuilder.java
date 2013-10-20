package org.erlide.core.internal.builder.external;

import java.util.Collection;

import org.erlide.core.ErlangCore;
import org.erlide.core.executor.ToolExecutor.ToolResults;
import org.erlide.core.internal.builder.ExternalBuilder;

public class MakeBuilder extends ExternalBuilder {

    @Override
    public String getOsCommand() {
        return "make";
    }

    /*
     * This is usable only for small, "normal" Makefiles.
     */
    Collection<String> getMakefileTargets(final String dir) {
        final ToolResults make = ex.run("/bin/bash",
                "-c \"make -rpn | sed -n -e '/^$/ { n ; /^[^ ]*:/p }' "
                        + "| grep -v ^.PHONY | cut -d : -f 1\"", getProject()
                        .getLocation().toPortableString());
        return make.output;
    }

    @Override
    public String getId() {
        return ErlangCore.PLUGIN_ID + ".make.builder";
    }

}
