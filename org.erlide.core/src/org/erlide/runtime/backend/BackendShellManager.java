/*******************************************************************************
 * Copyright (c) 2006 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime.backend;

import java.util.Collection;
import java.util.HashMap;

import org.erlide.jinterface.backend.IDisposable;

public class BackendShellManager implements IShellManager, IDisposable {

	private final HashMap<String, BackendShell> fShells;

	private final ErlideBackend fBackend;

	public BackendShellManager(final ErlideBackend backend) {
		fBackend = backend;
		fShells = new HashMap<String, BackendShell>();
	}

	/**
	 * @see org.erlide.runtime.backend.console.IShellManager#getShell(java.lang.String)
	 */
	public BackendShell getShell(final String id) {
		final BackendShell shell = fShells.get(id);
		return shell;
	}

	/**
	 * @see org.erlide.runtime.backend.console.IShellManager#openShell(java.lang.
	 *      String)
	 */
	public synchronized BackendShell openShell(final String id) {
		BackendShell shell = getShell(id);
		if (shell == null) {
			shell = new BackendShell(fBackend, id);
			fShells.put(id, shell);
		}
		return shell;
	}

	/**
	 * @see org.erlide.runtime.backend.console.IShellManager#closeShell(java.lang
	 *      .String)
	 */
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
