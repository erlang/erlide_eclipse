package org.erlide.engine

import org.eclipse.xtend.lib.annotations.ToString

@ToString
class ErlangInitializeParamsImpl extends ErlangInitializeParams {
    String stateDir

    override String getStateDir() {
        return stateDir
    }

    def void setStateDir(String stateDir) {
        this.stateDir = stateDir
    }
}
