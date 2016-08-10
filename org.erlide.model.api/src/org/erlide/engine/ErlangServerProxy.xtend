package org.erlide.engine

import org.eclipse.xtend.lib.annotations.Delegate

class ErlangServerProxy implements IErlangEngine {

    @Delegate
    IErlangEngine getToggleCommentService

    new(IErlangEngine server) {
        this.getToggleCommentService = server
    }

}
