package org.erlide.engine.internal

import org.eclipse.xtend.lib.annotations.Delegate
import org.erlide.engine.IErlangEngine
import org.eclipse.core.runtime.IExecutableExtension
import org.eclipse.core.runtime.IConfigurationElement
import org.eclipse.core.runtime.CoreException

class ErlangServerProxy implements IErlangEngine, IExecutableExtension {

    @Delegate
    IErlangEngine server

    new() {
        this(new ErlangServerImpl())
    }

    new(IErlangEngine server) {
        this.server = server
    }

    override setInitializationData(IConfigurationElement config, String propertyName, Object data) throws CoreException {
    }

}
