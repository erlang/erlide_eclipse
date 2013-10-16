package org.erlide.core.internal.builder;

import java.util.Collection;

import org.eclipse.core.runtime.IPath;
import org.erlide.core.ErlangCore;
import org.erlide.core.executor.ToolExecutor.ToolResults;
import org.erlide.engine.model.root.BuilderConfigParser;
import org.erlide.engine.model.root.IErlangProjectProperties;

public class MakeBuilder extends ExternalBuilder {

    @Override
    public String getOsCommand() {
        return "make";
    }

    Collection<String> getMakefileTargets(final String dir) {
        final ToolResults make = ex
                .run("/bin/bash",
                        "-c \"make -rpn | sed -n -e '/^$/ { n ; /^[^ ]*:/p }' | grep -v ^.PHONY | cut -d : -f 1\"",
                        getProject().getLocation().toPortableString());
        return make.output;
    }

    @Override
    public BuilderConfigParser getConfigParser() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void createConfig(final IPath location, final IErlangProjectProperties info) {
        // TODO Auto-generated method stub

    }

    @Override
    public String getId() {
        return ErlangCore.PLUGIN_ID + ".make.builder";
    }

}
