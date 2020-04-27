package org.erlide.engine;

import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

@SuppressWarnings("all")
public class ErlangInitializeParamsImpl extends ErlangInitializeParams {
    private String stateDir;

    @Override
    public String getStateDir() {
        return stateDir;
    }

    public void setStateDir(final String stateDir) {
        this.stateDir = stateDir;
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this).addAllFields().toString();
    }
}
