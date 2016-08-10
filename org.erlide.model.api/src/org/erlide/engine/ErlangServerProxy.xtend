package org.erlide.engine

import org.eclipse.xtend.lib.annotations.Delegate

class ErlangServerProxy implements IErlangEngine {

    @Delegate
    IErlangEngine server

    new(IErlangEngine server) {
        this.server = server
    }

}
