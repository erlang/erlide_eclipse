/*******************************************************************************
 * Copyright (c) 2009 * and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     *
 *******************************************************************************/
package org.erlide.backend.console;

import java.util.Collection;
import java.util.HashMap;

import org.erlide.backend.IBackend;
import org.erlide.jinterface.ErlLogger;
import org.erlide.utils.IDisposable;

import com.ericsson.otp.erlang.OtpErlangPid;

public class BackendShellManager implements IDisposable {

    private final IBackend backend;
    private final HashMap<String, BackendShell> fShells;

    public BackendShellManager(final IBackend backend) {
        this.backend = backend;
        fShells = new HashMap<String, BackendShell>();
    }

    public BackendShell getShell(final String id) {
        final BackendShell shell = fShells.get(id);
        return shell;
    }

    public synchronized IBackendShell openShell(final String id) {
        BackendShell shell = getShell(id);
        if (shell == null) {
            OtpErlangPid server = null;
            if (backend.isDistributed()) {
                try {
                    server = ErlideReshd.start(backend);
                } catch (final Exception e) {
                    ErlLogger.warn(e);
                }
            }
            shell = new BackendShell(backend, id, server);
            fShells.put(id, shell);
        }
        return shell;
    }

    public synchronized void closeShell(final String id) {
        final IBackendShell shell = getShell(id);
        if (shell != null) {
            fShells.remove(id);
            shell.close();
        }
    }

    @Override
    public void dispose() {
        final Collection<BackendShell> c = fShells.values();
        for (final IBackendShell backendShell : c) {
            backendShell.close();
        }
        fShells.clear();
    }
}
