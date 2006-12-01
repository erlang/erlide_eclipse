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
import java.util.Iterator;

import org.erlide.runtime.backend.IBackend;

public class BackendShellManager {

	private HashMap<String,BackendShell> fShells;

	private IBackend fBackend;

	public BackendShellManager(IBackend backend) {
		fBackend = backend;
		fShells = new HashMap<String,BackendShell>(5);
	}

	public BackendShell get(String id) {
		final BackendShell shell = (BackendShell) fShells.get(id);
		return shell;
	}

	public BackendShell openShell(String id) {
		BackendShell shell = get(id);
		if (shell == null) {
			shell = new BackendShell(fBackend, id);
			fShells.put(id, shell);
		}
		return shell;
	}

	public void closeShell(String id) {
		final BackendShell shell = get(id);
		if (shell != null) {
			fShells.remove(id);
			shell.close();
		}
	}

	public void dispose() {
		final Collection c = fShells.values();
		for (final Iterator iter = c.iterator(); iter.hasNext();) {
			final BackendShell element = (BackendShell) iter.next();
			element.close();
		}
	}

}
