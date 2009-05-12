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
package org.erlide.runtime.backend.console;

import java.util.Collection;
import java.util.HashMap;

import org.erlide.backend.Backend;
import org.erlide.backend.IDisposable;

public class BackendShellManager implements IShellManager, IDisposable {

	private final HashMap<String, BackendShell> fShells;

	private final Backend fBackend;

	public BackendShellManager(final Backend backend) {
		fBackend = backend;
		fShells = new HashMap<String, BackendShell>();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.erlide.runtime.backend.console.IShellManager#get(java.lang.String)
	 */
	public BackendShell get(final String id) {
		final BackendShell shell = fShells.get(id);
		return shell;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.erlide.runtime.backend.console.IShellManager#openShell(java.lang.
	 * String)
	 */
	public BackendShell openShell(final String id) {
		BackendShell shell = get(id);
		if (shell == null) {
			shell = new BackendShell(fBackend, id);
			fShells.put(id, shell);
		}
		return shell;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.erlide.runtime.backend.console.IShellManager#closeShell(java.lang
	 * .String)
	 */
	public void closeShell(final String id) {
		final BackendShell shell = get(id);
		if (shell != null) {
			fShells.remove(id);
			shell.close();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.erlide.runtime.backend.console.IDisposable#dispose()
	 */
	public void dispose() {
		final Collection<BackendShell> c = fShells.values();
		for (final BackendShell backendShell : c) {
			backendShell.close();
		}
	}

}
