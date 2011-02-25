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
package org.erlide.core.backend;

import java.util.Collection;
import java.util.HashMap;

import org.erlide.core.backend.console.BackendShell;
import org.erlide.core.common.IDisposable;

class BackendShellManager implements IDisposable {

    private final Backend backend;
    private final HashMap<String, BackendShell> fShells;

    public BackendShellManager(final Backend backend) {
        this.backend = backend;
        fShells = new HashMap<String, BackendShell>();
    }

    public BackendShell getShell(final String id) {
        final BackendShell shell = fShells.get(id);
        return shell;
    }

    public synchronized BackendShell openShell(final String id) {
        BackendShell shell = getShell(id);
        if (shell == null) {
            shell = new BackendShell(this.backend, id);
            fShells.put(id, shell);
        }
        return shell;
    }

    public synchronized void closeShell(final String id) {
        final BackendShell shell = getShell(id);
        if (shell != null) {
            fShells.remove(id);
            shell.close();
        }
    }

    public void dispose() {
        final Collection<BackendShell> c = fShells.values();
        for (final BackendShell backendShell : c) {
            backendShell.close();
        }
        fShells.clear();
    }
}
