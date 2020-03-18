package org.erlide.engine;

import org.eclipse.xtend.lib.annotations.ToString;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

@ToString
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
    @Pure
    public String toString() {
        return new ToStringBuilder(this).addAllFields().toString();
    }
}
