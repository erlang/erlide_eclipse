package org.erlide.engine

import org.eclipse.xtend.lib.annotations.ToString
import io.typefox.lsapi.impl.InitializeParamsImpl

@ToString
class ErlangInitializeParamsImpl extends InitializeParamsImpl implements ErlangInitializeParams {
    String stateDir

    override String getStateDir() {
        return stateDir
    }

    def void setStateDir(String stateDir) {
        this.stateDir = stateDir
    }
}
